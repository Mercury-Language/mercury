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
#define stringify(string) #string
#define entry(label) paste(entry_,label)

#if defined(USE_GCC_NONLOCAL_GOTOS)

  #ifndef __GNUC__
  #error "You must use gcc if you define USE_GCC_NONLOCAL_GOTOS"
  #endif

  #define BEGIN_MODULE(module_name)	\
	void module_name(void);	/* suppress gcc warning */	\
	void module_name(void) { {	\
	
	    /* initialization code for module goes here */

  #define BEGIN_CODE } return; {

	    /* body of module goes here */

  #define END_MODULE } }

  #if defined(USE_ASM_LABELS)

    #define Declare_entry(label)	\
	extern void label(void) __asm__("entry_" stringify(label))

    #define Define_entry(label)	\
	}	\
	label:	\
		__asm__(".globl entry_" stringify(label) "\n\t"	\
			"entry_" stringify(label) ":");	\
	{
    #include	"dummy.h"
    #define init_entry(label)	\
	/* prevent over-zealous optimization */	\
	volatile_global_pointer = &&label;	\
	makeentry(stringify(label), label)

    #define ENTRY(label) 	(&label)

  #else
    /* !defined(USE_ASM_LABELS) */

    #define Declare_entry(label)	\
	extern Code * entry(label)

    #define Define_entry(label)	\
	}	\
	label:	\
	{

    #define init_entry(label)	\
	makeentry(stringify(label), &&label);	\
	entry(label) = &&label

    #define ENTRY(label) 	(entry(label))

  #endif

  #define Declare_local(label)	/* no declaration required */
  #define Define_local(label)	\
	}	\
	label:	\
	{
  #define init_local(label)	make_local(stringify(label), label)

  #define Declare_label(label)	/* no declaration required */
  #define Define_label(label)	\
	}	\
	label:	\
	{
  #define init_label(label)	make_label(stringify(label), label)


  #define LOCAL(label)		(&&label)
  #define LABEL(label)		(&&label)
  #define GOTO(label)		do { debuggoto(label); goto *(label); } while(0)
  #define GOTO_ENTRY(label) 	GOTO(ENTRY(label))
  #define GOTO_LOCAL(label) 	GOTO_LABEL(label)
  #define GOTO_LABEL(label) 	do { debuggoto(&&label); goto label; } while(0)
  /*
  ** GOTO_LABEL(label) is the same as GOTO(LABEL(label)) except
  ** that it may allow gcc to generate slightly better code
  */

#else
  /* !defined(USE_GCC_NONLOCAL_GOTOS) */

  #define BEGIN_MODULE(module_name)	Code* module_name(void); \
					Code* module_name(void) {
  #define BEGIN_CODE			return 0;
  #define END_MODULE			}

  #define Declare_entry(label)	void *label(void);
  #define Define_entry(label)	\
		GOTO(label);	\
	}			\
	Code* label(void) {
  #define init_entry(label)	make_entry(stringify(label), label)

  #define Declare_local(label)	static Code *label(void);
  #define Define_local(label)	\
		GOTO(label);	\
	}			\
	static Code* label(void) {
  #define init_local(label)	make_local(stringify(label), label)

  #define Declare_label(label)	static Code *label(void);
  #define Define_label(label)	\
		GOTO(label);	\
	}			\
	static Code* label(void) {
  #define init_label(label)	make_label(stringify(label), label)

  #define ENTRY(label) 		(label)
  #define LOCAL(label)		(label)
  #define LABEL(label)		(label)
  #define GOTO(label)		return (label)
				/* the call to debuggoto() is in engine.mod */
  #define GOTO_ENTRY(label) 	GOTO(ENTRY(label))
  #define GOTO_LOCAL(label) 	GOTO(LOCAL(label))
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

#include	"engine.h"

/* DEFINITIONS FOR PROFILING */

#ifdef	USE_PROFILING

#include	"prof.h"
#define	PROFILE(callee, caller)		prof_call_profile((callee), (caller))

#else

#define	PROFILE(callee, caller)		((void)0)

#endif

/* DEFINITIONS FOR CALLS AND RETURNS */

#define	localcall(label, succ_cont, current_label)		\
			do {					\
				debugcall(LABEL(label), (succ_cont)); \
				succip = (succ_cont);		\
				PROFILE(LABEL(label), (current_label));	\
				GOTO_LABEL(label);		\
			} while (0)

#define	call(proc, succ_cont, current_label)			\
			do {					\
				debugcall((proc), (succ_cont));	\
				succip = (succ_cont);		\
				PROFILE((proc), (current_label));	\
				GOTO(proc);			\
			} while (0)

#define	call_closure(succ_cont)					\
		do {						\
			Declare_entry(do_call_closure); \
			call(ENTRY(do_call_closure), succ_cont); \
		} while (0)

#define	call_semidet_closure(succ_cont)				\
		do {						\
			Declare_entry(do_call_semidet_closure); \
			call(ENTRY(do_call_semidet_closure), succ_cont); \
		} while (0)

#define	solutions(succ_cont)					\
		do {						\
			Declare_entry(do_solutions);		\
			call(ENTRY(do_solutions), succ_cont); 	\
		} while (0)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define	callentry(procname, succ_cont)				\
			do {					\
				extern EntryPoint ENTRY(procname); \
				call(ENTRY(procname), succ_cont); \
			} while (0)

#define	localtailcall(label, current_label)			\
			do {					\
				debugtailcall(LABEL(label));	\
				PROFILE(LABEL(label), (current_label)); \
				GOTO_LABEL(label);		\
			} while (0)
#define	tailcall(proc, current_label)	do {			\
				debugtailcall(proc);		\
				PROFILE((proc), (current_label)); \
				GOTO(proc);			\
			} while (0)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define	tailcallentry(procname, current_label)			\
			do {					\
				extern EntryPoint ENTRY(procname); \
				tailcall(ENTRY(procname), (current_label)); \
			} while (0)

#define	proceed()	do {					\
				debugproceed();			\
				GOTO(succip);			\
			} while (0)

/* DEFINITIONS FOR MANIPULATING THE HEAP */

#ifdef CONSERVATIVE_GC

#include "gc.h"

#define	tag_incr_hp(dest,tag,count) \
	((dest) = mkword(tag, (Word)GC_MALLOC(count * sizeof(Word))))
#define	mark_hp(dest)	((void)0)
#define	restore_hp(src)	((void)0)
#define hp_alloc(count) (incr_hp(hp,(count)), hp += (count), (void)0)
			/* we use `hp' as a convenient temporary here */

#else

#define	tag_incr_hp(dest,tag,count)	(			\
				(dest) = mkword(tag, (Word)hp),	\
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

#define hp_alloc(count)  incr_hp(hp,count)

#endif

#define	incr_hp(dest,count)	tag_incr_hp((dest),mktag(0),(count))

/*
** Note that gcc optimizes `hp += 2; return hp - 2;'
** to `tmp = hp; hp += 2; return tmp;', so we don't need to use
** gcc's expression statements here
*/

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create1(w1)	(					\
				hp_alloc(1),			\
				hp[-1] = (Word) (w1),		\
				debugcr1(hp[-1], hp),		\
				/* return */ (Word) (hp - 1)	\
			)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create2(w1, w2)	(					\
				hp_alloc(2),			\
				hp[-2] = (Word) (w1),		\
				hp[-1] = (Word) (w2),		\
				debugcr2(hp[-2], hp[-1], hp),	\
				/* return */ (Word) (hp - 2)	\
			)

/* used only by the hand-written example programs */
/* not by the automatically generated code */
#define create3(w1, w2, w3)	(				\
				hp_alloc(3),			\
				hp[-3] = (Word) (w1),		\
				hp[-2] = (Word) (w2),		\
				hp[-1] = (Word) (w3),		\
				/* return */ (Word) (hp - 3)	\
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

#define IF(cond, val)	((cond) ? (val) : 0)

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

#if defined(SPEED) && !defined(DEBUG_GOTOS)

#define	debuggoto(label)			((void)0)
#define	debugsreg()				((void)0)

#else

#define	debuggoto(label) \
	(assert(label), \
	IF (gotodebug, (save_transient_registers(), goto_msg(label))))

#define	debugsreg() \
	IF (sregdebug, (save_transient_registers(), reg_msg()))

#endif

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
#define	debugcall(proc, succ_cont)		((void)0)
#define	debugtailcall(proc)			((void)0)
#define	debugproceed()				((void)0)
#define	debugmsg0(msg)				((void)0)
#define	debugmsg1(msg, arg1)			((void)0)
#define	debugmsg2(msg, arg1, arg2)		((void)0)
#define	debugmsg3(msg, arg1, arg2, arg3)	((void)0)

#else

#define	debugcr1(val0, hp) \
	IF (heapdebug, (save_transient_registers(), cr1_msg(val0, hp)))

#define	debugcr2(val0, val1, hp) \
	IF (heapdebug, (save_transient_registers(), cr2_msg(val0, val1, hp)))

#define	debugincrhp(val, hp) \
	IF (heapdebug, (save_transient_registers(), incr_hp_msg((val), (hp))))

#define	debugincrsp(val, sp) \
	IF (detstackdebug, (save_transient_registers(), incr_sp_msg((val), (sp))))

#define	debugdecrsp(val, sp) \
	IF (detstackdebug, (save_transient_registers(), decr_sp_msg((val), (sp))))

#define	debugpush(val, sp) \
	IF (detstackdebug, (save_transient_registers(), push_msg((val), (sp))))

#define	debugpop(val, sp) \
	IF (detstackdebug, (save_transient_registers(), pop_msg(val, sp)))

#define	debugregs(msg) \
	IF (progdebug, (save_transient_registers(), printregs(msg)))

#define	debugmkframe() \
	IF (nondstackdebug, (save_transient_registers(), mkframe_msg()))

#define	debugframe(msg)	 \
	IF (progdebug, (save_transient_registers(), printframe(msg)))

#define	debugmodframe() \
	IF (nondstackdebug, (save_transient_registers(), modframe_msg()))

#define	debugsucceed() \
	IF (nondstackdebug, (save_transient_registers(), succeed_msg()))

#define	debugsucceeddiscard() \
	IF (nondstackdebug, (save_transient_registers(), succeeddiscard_msg()))

#define	debugfail() \
	IF (nondstackdebug, (save_transient_registers(), fail_msg()))

#define	debugredo() \
	IF (nondstackdebug, (save_transient_registers(), redo_msg()))

#define	debugcall(proc, succ_cont) \
	IF (calldebug, (save_transient_registers(), call_msg(proc, succ_cont)))

#define	debugtailcall(proc) \
	IF (calldebug, (save_transient_registers(), tailcall_msg(proc)))

#define	debugproceed() \
	IF (calldebug, (save_transient_registers(), proceed_msg()))

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

/* DEFINITIONS TO SUPPORT DEBUGGING */

#ifdef __GNUC__
#define mklist(start,len) \
	({						\
		Word tmp;				\
		save_transient_registers();			\
		tmp = do_mklist(start,len);		\
		restore_transient_registers();			\
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
