/*
** Copyright (C) 1995-1997 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* goto.h - definitions for the "portable assembler" non-local gotos */

#ifndef GOTO_H
#define GOTO_H

#include "mercury_types.h"	/* for `Code *' */
#include "debug.h"		/* for debuggoto() */

/*
** Taking the address of a label can inhibit gcc's optimization,
** because it assumes that anything can jump there.
** Therefore we want to do it only if we're debugging,
** or if we need the label address for profiling.
*/

#if defined(SPEED) && !defined(DEBUG_GOTOS)
#define	make_label(n, a)	/* nothing */
#else
#define	make_label(n, a)	make_entry(n, a)
#endif

#if defined(SPEED) && !defined(DEBUG_GOTOS) && !defined(PROFILE_CALLS)
#define make_local(n, a)	/* nothing */
#else 
#define make_local(n, a)	make_entry(n, a)
#endif

#if defined(SPEED) && !defined(DEBUG_LABELS) && !defined(DEBUG_GOTOS) \
			&& !defined(PROFILE_CALLS)
#define make_entry(n, a)	/* nothing */
#else
#define make_entry(n, a)	insert_entry(n, a)
#endif

#define paste(a,b) a##b
#define stringify(string) #string
#define entry(label) paste(entry_,label)
#define skip(label) paste(skip_,label)

#ifdef SPLIT_C_FILES
#define MODULE_STATIC_OR_EXTERN extern
#else
#define MODULE_STATIC_OR_EXTERN static
#endif

/*---------------------------------------------------------------------------*/

/* MACHINE SPECIFIC STUFF REQUIRED FOR NON-LOCAL GOTOS */

#if defined(__alpha__)

  /* We need special handling for the "global pointer" (gp) register. */

  /*
  ** When doing a jump, we need to set $27, the "procedure value" register,
  ** to the address we are jumping to, so that we can use an `ldgp'
  ** instruction on entry to the procedure to set up the right gp value.
  */
  #define ASM_JUMP(address)				\
	__asm__("bis %0, %0, $27\n\t" 			\
		: : "r"(address) : "$27");		\
	goto *(address)
	/*
	** Explanation:
	**	Move `address' to register $27,
	**	jump to `address'.
	*/

  /*
  ** on entry to a procedure, we need to load the $gp register
  ** with the correct value relative to the current address in $27
  */
  #define INLINE_ASM_FIXUP_REGS				\
  	"	ldgp $gp, 0($27)\n" : : : "memory"

  /*
  ** on fall-thru, we need to skip the ldgp instruction
  */
  #define ASM_FALLTHROUGH(label) \
  	goto skip(label);

#elif defined(__i386__)

  /*
  ** The following hack works around a stack leak on the i386.
  ** The problem is that gcc pushes function parameters onto
  ** the stack when calling C functions such as GC_malloc(),
  ** and only restores the stack pointer in the epilogue.
  ** With non-local gotos, we jump out of the function without
  ** executing its epilogue, so the stack pointer never gets
  ** restored.  The result is a memory leak; for example,
  ** `mc --generate-dependencies mercury_compile' exceeds the
  ** Slackware Linux default stack space limit of 8M.
  **
  ** GNU C has an option `-fno-defer-pop' which is supposed to
  ** avoid this sort of thing, but it doesn't work for our
  ** code using non-local gotos.
  **
  ** We work around this using the dummy assembler code below, which
  ** pretends to use the stack pointer, forcing gcc to flush any updates
  ** of the stack pointer immediately, rather than deferring them until
  ** the function epilogue.
  **
  ** I know this is awful.  It wasn't _my_ idea to use non-local gotos ;-)
  */
  #define ASM_JUMP(label)				\
  	{ register int stack_pointer __asm__("esp");	\
  	__asm__("" : : "g"(stack_pointer)); }		\
  	goto *(label)

  /*
  ** That hack above needs to be done for all non-local jumps,
  ** even if we're not using assembler labels.
  */
  #define JUMP(label)		ASM_JUMP(label)

  /*
  ** If we're using position-independent code on i386, then we need to
  ** set up the correct value of the GOT register (ebx).
  */

  #if (defined(__pic__) || defined(__PIC__)) && !defined(PIC)
    #define PIC 1
  #endif

  #if PIC

    /*
    ** At each entry point, where we may have been jump to from
    ** code in a difference C file, we need to set up `ebx'. 
    ** We do this by pushing the IP register using a `call'
    ** instruction whose target is the very next label.
    ** We then pop this off the stack into `ebx', and
    ** then use the value obtained to compute the correct
    ** value of `ebx' by doing something with _GLOBAL_OFFSET_TABLE_
    ** (I don't understand the details exactly, this code is
    ** basically copied from the output of `gcc -fpic -S'.)
    ** Note that `0f' means the label `0:' following the current
    ** instruction, and `0b' means the label `0:' before the current
    ** instruction.
    **
    ** Note: this code clobbers `ebx', which is a callee-save
    ** register.  That means that it is essential that call_engine()
    ** save `ebx' before entering Mercury code, and restore it
    ** before returning to C code.  However, gcc and/or
    ** setjmp()/longjmp() will do that for us automatically,
    ** precisely because it is a callee-save register.
    */
    #define INLINE_ASM_FIXUP_REGS     				\
    	"	call 0f\n"     					\
    	"0:\n"       						\
    	"	popl %%ebx\n"     				\
    	"	addl $_GLOBAL_OFFSET_TABLE_+[.-0b],%%ebx\n\t"	\
    		: :
#if 0
	/*
	** The following doesn't seem to be necessary, and
	** leaving it out might make gcc generate slightly better code.
	*/
		/* tell gcc we clobber ebx and memory */	\
    		: : : "%ebx", "memory"
#endif

    /*
    ** It is safe to fall through into INLINE_ASM_FIXUP_REGS,
    ** but it might be more efficient to branch past it.
    ** We should really measure both ways and find out which is
    ** better... for the moment, we just fall through, since
    ** that keeps the code smaller.
    */
    #if 0
      #define ASM_FALLTHROUGH(label) \
  	goto skip(label);
    #endif

  #endif /* PIC */

  /* For Linux-ELF shared libraries, we need to declare that the type of labels
  ** is @function (i.e. code, not data), otherwise the dynamic linker seems
  ** to get confused, and we end up jumping into the data section.
  ** Hence the `.type' directive below.
  */
  #ifdef __ELF__
    #define INLINE_ASM_ENTRY_LABEL_TYPE(label) \
	"	.type entry_" stringify(label) ",@function\n"
  #endif

#elif defined (__sparc)

  /*
  ** If we're using position-independent code on the SPARC, then we need to
  ** set up the correct value of the GOT register (l7).
  */

  #if (defined(__pic__) || defined(__PIC__)) && !defined(PIC)
    #define PIC 1
  #endif

  #if PIC

    /*
    ** At each entry point, where we may have been jump to from
    ** code in a difference C file, we need to set up `l7'. 
    ** We do this by getting the value the of the IP register using a `call'
    ** instruction whose target is the very next label; this will
    ** put the address of the call instruction in register `o7'.
    ** We then use the value obtained in register `o7' to compute the correct
    ** value of register `l7' by doing something with _GLOBAL_OFFSET_TABLE_
    ** (I don't understand the details exactly, this code is
    ** basically copied from the output of `gcc -fpic -S'.)
    ** Note that `1f' means the label `1:' following the current
    ** instruction, and `0b' means the label `0:' before the current
    ** instruction.
    */
    #define INLINE_ASM_FIXUP_REGS     				\
    	"0:\n"     						\
    	"	call 1f\n"     					\
    	"	nop\n"     					\
    	"1:\n"       						\
	"	sethi %%hi(_GLOBAL_OFFSET_TABLE_-(0b-.)),%%l7\n"	\
	"	or %%l7,%%lo(_GLOBAL_OFFSET_TABLE_-(0b-.)),%%l7\n"	\
	"	add %%l7,%%o7,%%l7\n"				\
		/* tell gcc we clobber l7, o7, and memory */	\
    		: : : "%l7", "%o7", "memory"

    /*
    ** It is safe to fall through into INLINE_ASM_FIXUP_REGS,
    ** but it might be more efficient to branch past it.
    ** We should really measure both ways and find out which is
    ** better... for the moment, we just fall through, since
    ** that keeps the code smaller.
    */
    #if 0
      #define ASM_FALLTHROUGH(label) \
  	goto skip(label);
    #endif

  #endif /* PIC */

  /* For Solaris 5.5.1, we need to declare that the type of labels is
  ** #function (i.e. code, not data), otherwise the dynamic linker seems
  ** to get confused, and we end up jumping into the data section.
  ** Hence the `.type' directive below.
  */
  #define INLINE_ASM_ENTRY_LABEL_TYPE(label) \
	"	.type entry_" stringify(label) ",#function\n"

#endif

/* for other architectures, these macros have default definitions */

/*
** INLINE_ASM_FIXUP_REGS is used to fix up the value of
** any registers after an ASM_JUMP to an entry point, if necessary.
** It is an assembler string, possibly followed by `: : : <clobbers>'
** where <clobbers> is an indication to gcc of what gets clobbered.
*/
#ifdef INLINE_ASM_FIXUP_REGS
#define ASM_FIXUP_REGS					\
	__asm__ __volatile__(				\
		INLINE_ASM_FIXUP_REGS			\
	);
#define NEED_ASM_FIXUP_REGS
#else
#define ASM_FIXUP_REGS
#define INLINE_ASM_FIXUP_REGS
#undef  NEED_ASM_FIXUP_REGS
#endif

/*
** ASM_FALLTHROUGH is executed when falling through into an entry point.
** It may call `goto skip(label)' if it wishes to skip past the
** label and the INLINE_ASM_FIXUP_REGS code.
*/
#ifndef ASM_FALLTHROUGH
#define ASM_FALLTHROUGH(label)
#endif

/*
** INLINE_ASM_GLOBALIZE_LABEL is an assembler string to
** declare an entry label as global.  The following definition
** using `.globl' should work with the GNU assembler and
** with most Unix assemblers.
*/
#ifndef INLINE_ASM_GLOBALIZE_LABEL
#define INLINE_ASM_GLOBALIZE_LABEL(label) \
	"	.globl entry_" stringify(label) "\n"
#endif

/*
** INLINE_ASM_ENTRY_LABEL_TYPE is an assembler string to
** declare the "type" of a label as function (i.e. code), not data,
** if this is needed.
*/
#ifndef INLINE_ASM_ENTRY_LABEL_TYPE
#define INLINE_ASM_ENTRY_LABEL_TYPE(label)
#endif

/*
** INLINE_ASM_ENTRY_LABEL is an assembler string to
** define an assembler entry label.
*/
#ifndef INLINE_ASM_ENTRY_LABEL
#define INLINE_ASM_ENTRY_LABEL(label)	\
	"entry_" stringify(label) ":\n"
#endif

/*
** ASM_JUMP is used to jump to an entry point defined with
** ASM_ENTRY, ASM_STATIC_ENTRY, or ASM_LOCAL_ENTRY.
*/
#ifndef ASM_JUMP
#define ASM_JUMP(address)	goto *(address)
#endif

/*---------------------------------------------------------------------------*/

/* The code from here on should be architecture-independent. */

/*---------------------------------------------------------------------------*/

/*
** ASM_ENTRY is used to declare an external entry point
** using a (global) assembler label.
*/
#define ASM_ENTRY(label) 				\
	ASM_FALLTHROUGH(label)				\
  	entry(label):					\
	__asm__ __volatile__(				\
		INLINE_ASM_GLOBALIZE_LABEL(label)	\
		INLINE_ASM_ENTRY_LABEL_TYPE(label)	\
		INLINE_ASM_ENTRY_LABEL(label)		\
		INLINE_ASM_FIXUP_REGS			\
	);						\
	skip(label): ;

/*
** ASM_STATIC_ENTRY is the same as ASM_ENTRY,
** except that its scope is local to a C file, rather than global.
** Note that even static entry points must do INLINE_ASM_FIXUP_REGS,
** since although there won't be any direct calls to them from another
** C file, their address may be taken and so there may be indirect
** calls.
*/
#define ASM_STATIC_ENTRY(label) 			\
	ASM_FALLTHROUGH(label)				\
  	entry(label):					\
	__asm__ __volatile__(				\
		INLINE_ASM_ENTRY_LABEL_TYPE(label)	\
		INLINE_ASM_ENTRY_LABEL(label)		\
		INLINE_ASM_FIXUP_REGS			\
	);						\
	skip(label): ;

/*
** ASM_LOCAL_ENTRY is the same as ASM_ENTRY,
** except that its scope is local to a BEGIN_MODULE...END_MODULE
** block, rather than being global.
** Note that even local entry points must do INLINE_ASM_FIXUP_REGS, since
** although there won't be any direct calls to them from another
** C file, their address may be taken and so there may be indirect
** calls.
*/
#define ASM_LOCAL_ENTRY(label) 				\
	ASM_FALLTHROUGH(label)				\
  	entry(label):					\
  	ASM_FIXUP_REGS					\
	skip(label): ;

/*---------------------------------------------------------------------------*/

#if defined(USE_GCC_NONLOCAL_GOTOS)

  #ifndef __GNUC__
  #error "You must use gcc if you define USE_GCC_NONLOCAL_GOTOS"
  #endif

  /* Define the type of a module initialization function */
  typedef void ModuleFunc(void);

  /* The following macro expands to a dummy assembler statement which
  ** contains no code, but which tells gcc that it uses the specified
  ** address as an input value.  This is used to trick gcc into
  ** thinking that the address is used, in order to suppress unwanted
  ** optimizations.  (We used to use `volatile_global_pointer =
  ** address' to suppress optimization, but this way is better because
  ** it doesn't generate any code.)
  */
  #define PRETEND_ADDRESS_IS_USED(address)		\
	__asm__ __volatile__("" : : "g"(address))
  /*
  Explanation:
  	__asm__
  	__volatile__			don't optimize this asm away
  	(
  		""			empty assembler code
  		: 			no outputs
  		: "g" (address)		one input value, `address';
  					"g" means that it can go in any
  					general-purpose register
  	)
  */


  /* Since we're jumping into and out of the middle of functions,
  ** we need to make sure that gcc thinks that (1) the function's address
  ** is used (otherwise it may optimize the whole function away) and
  ** (2) the `return' statement is reachable (otherwise its dataflow
  ** analysis for delay slot scheduling may think that global
  ** register variables which are only assigned to in the function
  ** cannot be live, when in fact they really are).
  ** That is what the two occurrences of the PRETEND_ADDRESS_IS_USED
  ** macro are for.
  */
  #define BEGIN_MODULE(module_name)	\
	MODULE_STATIC_OR_EXTERN void module_name(void); \
	MODULE_STATIC_OR_EXTERN void module_name(void) { \
		PRETEND_ADDRESS_IS_USED(module_name); \
		PRETEND_ADDRESS_IS_USED(&& paste(module_name, _dummy_label)); \
		paste(module_name,_dummy_label): \
		{
  /* initialization code for module goes here */
  #define BEGIN_CODE } return; {
  /* body of module goes here */
  #define END_MODULE } }


  #if defined(USE_ASM_LABELS)
    #define Declare_entry(label)	\
	extern void label(void) __asm__("entry_" stringify(label))
    #define Declare_static(label)	\
	static void label(void) __asm__("entry_" stringify(label))
    #define Define_extern_entry(label)	Declare_entry(label)
    #define Define_entry(label)		\
		ASM_ENTRY(label)	\
	}				\
	label:				\
	PRETEND_ADDRESS_IS_USED(&&entry(label));	\
	{
    #define Define_static(label)	\
		ASM_STATIC_ENTRY(label) \
	}				\
	label:				\
	PRETEND_ADDRESS_IS_USED(&&entry(label));	\
	{
    /*
    ** The PRETEND_ADDRESS_IS_USED macro is necessary to 
    ** prevent an over-zealous gcc from optimizing away `label'
    ** and the code that followed. 
    */
    #define init_entry(label)	\
	PRETEND_ADDRESS_IS_USED(&&label); \
	make_entry(stringify(label), label)

    #define ENTRY(label) 	(&label)
    #define STATIC(label) 	(&label)

    #ifndef JUMP
    #define JUMP(label)		ASM_JUMP(label)
    #endif

  #else
    /* !defined(USE_ASM_LABELS) */

    #define Declare_entry(label)	extern Code * entry(label)
    #define Declare_static(label)	static Code * entry(label)
    #define Define_extern_entry(label)	Code * entry(label)
    #define Define_entry(label)	\
	}	\
	entry(label): \
	label:	\
	{
    #define Define_static(label)	\
	}	\
	entry(label): \
	label:	\
	{
    #define init_entry(label)	\
	make_entry(stringify(label), &&label);	\
	entry(label) = &&label
    #define ENTRY(label) 	(entry(label))
    #define STATIC(label) 	(entry(label))

    #ifndef JUMP
    #define JUMP(label)		goto *(label)
    #endif

  #endif /* !defined(USE_ASM_LABELS) */

  #define Declare_local(label)	/* no declaration required */
  #define Define_local(label)	\
  		ASM_LOCAL_ENTRY(label)	\
	}	\
	label:	\
	{
  #define init_local(label)	make_local(stringify(label), &&label)
  #define Declare_label(label)	/* no declaration required */
  #define Define_label(label)	Define_local(label)
  #define init_label(label)	make_label(stringify(label), &&label)

  #define LOCAL(label)		(&&entry(label))
  #define LABEL(label)		(&&entry(label))
  #define GOTO(label)		do { debuggoto(label); JUMP(label); } while(0)
  #define GOTO_ENTRY(label) 	GOTO(ENTRY(label))
  #define GOTO_STATIC(label) 	GOTO(STATIC(label))
  #define GOTO_LOCAL(label) 	GOTO_LABEL(label)
  #define GOTO_LABEL(label) 	do { debuggoto(&&label); goto label; } while(0)

  /*
  ** GOTO_LABEL(label) is the same as GOTO(LABEL(label)) except
  ** that it may allow gcc to generate slightly better code
  */

#else
  /* !defined(USE_GCC_NONLOCAL_GOTOS) */

  /* Define the type of a module initialization function */
  typedef Code * ModuleFunc(void);

  #define BEGIN_MODULE(module_name)	\
		MODULE_STATIC_OR_EXTERN Code* module_name(void); \
		MODULE_STATIC_OR_EXTERN Code* module_name(void) {
  #define BEGIN_CODE			return 0;
  #define END_MODULE			}

  #define Declare_entry(label)		extern void *label(void)
  #define Declare_static(label)		static void *label(void)
  #define Define_extern_entry(label)	void *label(void)
  #define Define_entry(label)	\
		GOTO(label);	\
	}			\
	Code* label(void) {
  #define Define_static(label)	\
		GOTO(label);	\
	}			\
	static Code* label(void) {
  #define init_entry(label)	make_entry(stringify(label), label)

  #define Declare_local(label)	static Code *label(void)
  #define Define_local(label)	\
		GOTO(label);	\
	}			\
	static Code* label(void) {
  #define init_local(label)	make_local(stringify(label), label)

  #define Declare_label(label)	static Code *label(void)
  #define Define_label(label)	\
		GOTO(label);	\
	}			\
	static Code* label(void) {
  #define init_label(label)	make_label(stringify(label), label)

  #define ENTRY(label) 		(label)
  #define STATIC(label) 	(label)
  #define LOCAL(label)		(label)
  #define LABEL(label)		(label)
  #define GOTO(label)		return (label)
				/* the call to debuggoto() is in engine.mod */
  #define GOTO_ENTRY(label) 	GOTO(ENTRY(label))
  #define GOTO_STATIC(label) 	GOTO(STATIC(label))
  #define GOTO_LOCAL(label) 	GOTO(LOCAL(label))
  #define GOTO_LABEL(label) 	GOTO(LABEL(label))

#endif /* !defined(USE_GCC_NONLOCAL_GOTOS) */

/* definitions for computed gotos */

#define COMPUTED_GOTO(val, labels) 			\
	{ static Code *jump_table[] = {			\
		labels					\
	  };						\
	  GOTO(jump_table[val]);			\
	}
#define AND ,	/* used to separate the labels */

#endif /* not GOTO_H */
