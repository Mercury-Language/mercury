/*
** Copyright (C) 1995-2001 The University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* mercury_goto.h - definitions for the "portable assembler" non-local gotos */

#ifndef MERCURY_GOTO_H
#define MERCURY_GOTO_H

#include "mercury_conf.h"
#include "mercury_std.h"	/* for MR_PASTE2 and MR_STRINGIFY */
#include "mercury_types.h"	/* for `MR_Code *' */
#include "mercury_debug.h"	/* for MR_debuggoto() */
#include "mercury_label.h"	/* for MR_insert_{entry,internal}_label() */
#include "mercury_dummy.h"	/* for MR_dummy_identify_function() */

#define MR_entry(label)		MR_PASTE2(_entry_,label)
#define MR_skip(label)		MR_PASTE2(skip_,label)

#define MR_PROC_LAYOUT(label)	(const MR_Proc_Layout *) (MR_Word) \
				&(MR_PASTE2(mercury_data__proc_layout__,label))
#define MR_LABEL_LAYOUT(label) 	(const MR_Label_Layout *) (MR_Word) \
				&(MR_PASTE2(mercury_data__label_layout__,label))

/*
** Passing the name of a label to MR_insert_{internal,entry}_label
** causes that name to be included in the executable as static readonly data.
** Since label names are quite big, we include them only when needed.
*/

#if defined(MR_INSERT_INTERNAL_LABEL_NAMES)
  #define MR_insert_internal(n, a, l)	MR_insert_internal_label(n, a, l)
#else
  #define MR_insert_internal(n, a, l)	MR_insert_internal_label(NULL, a, l)
#endif

#if defined(MR_INSERT_ENTRY_LABEL_NAMES)
  #define MR_insert_entry(n, a, l)	MR_insert_entry_label(n, a, l)
#else
  #define MR_insert_entry(n, a, l)	MR_insert_entry_label(NULL, a, l)
#endif

/*
** Taking the address of a label can inhibit gcc's optimization,
** because it assumes that anything can jump there.
** Therefore we want to do it only if we're debugging,
** or if we need the label address for profiling or
** accurate garbage collection.
**
** The versions of the macros below with the _ai or _sl suffix always insert
** the label into the label table, the difference between them being that
** the _ai variant does not include a layout structure. If the label *has*
** a layout structure, use the _sl variant.
*/

#define MR_make_label_ai(n, a, l)		MR_insert_internal(n, a, NULL)
#define MR_make_label_sl(n, a, l)		MR_insert_internal(n, a, \
							MR_LABEL_LAYOUT(l))

#define MR_make_local_ai(n, a, l)		MR_insert_entry(n, a, NULL)
#define MR_make_local_sl(n, a, l)		MR_insert_entry(n, a, \
							MR_PROC_LAYOUT(l))

#define MR_make_entry_ai(n, a, l)		MR_insert_entry(n, a, NULL)
#define MR_make_entry_sl(n, a, l)		MR_insert_entry(n, a, \
							MR_PROC_LAYOUT(l))

#if defined(MR_INSERT_LABELS)
  #define MR_make_label(n, a, l)		MR_make_label_ai(n, a, l)
#else
  #define MR_make_label(n, a, l)		/* nothing */
#endif

#if defined(MR_INSERT_LABELS) || defined(MR_MPROF_PROFILE_CALLS)
  #define MR_make_local(n, a, l)		MR_make_local_ai(n, a, l)
#else 
  #define MR_make_local(n, a, l)		/* nothing */
#endif

/*
** Note that for the MLDS back-end, the calls to MR_init_entry(),
** which eventually expand to make_entry(), are only output if
** the right compiler options are enabled.  So if you change the
** condition of this `#ifdef', and you want your changes to apply
** to the MLDS back-end too, you may also need to change the
** `need_to_init_entries' predicate in compiler/mlds_to_c.m.
*/
#if defined(MR_INSERT_LABELS) || defined(MR_MPROF_PROFILE_CALLS)
  #define MR_make_entry(n, a, l)		MR_make_entry_ai(n, a, l)
#else
  #define MR_make_entry(n, a, l)		/* nothing */
#endif

#ifdef SPLIT_C_FILES
  #define MR_MODULE_STATIC_OR_EXTERN extern
#else
  #define MR_MODULE_STATIC_OR_EXTERN static
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
  #define MR_ASM_JUMP(address)				\
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
  #define MR_INLINE_ASM_FIXUP_REGS				\
  	"	ldgp $gp, 0($27)\n" : : : "memory"

  /*
  ** on fall-thru, we need to skip the ldgp instruction
  */
  #define MR_ASM_FALLTHROUGH(label) \
  	goto MR_skip(label);

#elif defined(__i386__) || defined(__mc68000__)

  /*
  ** The following hack works around a stack leak on the i386.
  ** (and apparently the 68000 too).
  **
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
  #if defined(__i386__)
    #define MR_ASM_JUMP(label)				\
  	{ register int stack_pointer __asm__("esp");	\
  	__asm__("" : : "g"(stack_pointer)); }		\
  	goto *(label)
  #elif defined(__mc68000__)
    #define MR_ASM_JUMP(label)				\
  	{ register int stack_pointer __asm__("sp");	\
  	__asm__("" : : "g"(stack_pointer)); }		\
  	goto *(label)
  #endif

  /*
  ** That hack above needs to be done for all non-local jumps,
  ** even if we're not using assembler labels.
  */
  #define MR_JUMP(label)	MR_ASM_JUMP(label)

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
    #if defined(__i386__)

      #define MR_EBX "%%ebx"

      #define MR_INLINE_ASM_FIXUP_REGS     				\
    	"	call 0f\n"     						\
    	"0:\n"       							\
    	"	popl " MR_EBX "\n"     					\
    	"	addl $_GLOBAL_OFFSET_TABLE_+[.-0b]," MR_EBX "\n\t"	\
    		: :
#if 0
	/*
	** The following doesn't seem to be necessary, and
	** leaving it out might make gcc generate slightly better code.
	*/
		/* tell gcc we clobber ebx and memory */	\
    		: : : "%ebx", "memory"
#endif
    #elif defined(__mc68000__)

	/*
	**  This piece of magic thanks to Roman Hodek
	**  <Roman.Hodek@informatik.uni-erlangen.de>
	*/ 

      #define MR_INLINE_ASM_FIXUP_REGS \
        "       lea (%%pc,_GLOBAL_OFFSET_TABLE_@GOTPC),%%a5\n" : : : "memory"

    #endif

    /*
    ** It is safe to fall through into MR_INLINE_ASM_FIXUP_REGS,
    ** but it might be more efficient to branch past it.
    ** We should really measure both ways and find out which is
    ** better... for the moment, we just fall through, since
    ** that keeps the code smaller.
    */
    #if 0
      #define MR_ASM_FALLTHROUGH(label) \
  	goto MR_skip(label);
    #endif

  #endif /* PIC */

  /* For Linux-ELF shared libraries, we need to declare that the type of labels
  ** is @function (i.e. code, not data), otherwise the dynamic linker seems
  ** to get confused, and we end up jumping into the data section.
  ** Hence the `.type' directive below.
  */
  #ifdef __ELF__
    #define MR_INLINE_ASM_ENTRY_LABEL_TYPE(label) \
	"	.type _entry_" MR_STRINGIFY(label) ",@function\n"
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
    #define MR_INLINE_ASM_FIXUP_REGS     			\
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
    ** It is safe to fall through into MR_INLINE_ASM_FIXUP_REGS,
    ** but it might be more efficient to branch past it.
    ** We should really measure both ways and find out which is
    ** better... for the moment, we just fall through, since
    ** that keeps the code smaller.
    */
    #if 0
      #define MR_ASM_FALLTHROUGH(label) \
  	goto MR_skip(label);
    #endif

  #endif /* PIC */

  /*
  ** For Solaris 5.5.1, we need to declare that the type of labels is
  ** #function (i.e. code, not data), otherwise the dynamic linker seems
  ** to get confused, and we end up jumping into the data section.
  ** Hence the `.type' directive below.
  */
  #ifndef MR_CANNOT_GROK_ASM_TYPE_DIRECTIVE
    #define MR_INLINE_ASM_ENTRY_LABEL_TYPE(label) \
	"	.type _entry_" MR_STRINGIFY(label) ",#function\n"
  #endif

#endif

/* for other architectures, these macros have default definitions */

/*
** MR_INLINE_ASM_FIXUP_REGS is used to fix up the value of
** any registers after an MR_ASM_JUMP to an entry point, if necessary.
** It is an assembler string, possibly followed by `: : : <clobbers>'
** where <clobbers> is an indication to gcc of what gets clobbered.
*/
#ifdef MR_INLINE_ASM_FIXUP_REGS
  #define MR_ASM_FIXUP_REGS				\
	__asm__ __volatile__(				\
		MR_INLINE_ASM_FIXUP_REGS		\
	);
#else
  #define MR_ASM_FIXUP_REGS
  #define MR_INLINE_ASM_FIXUP_REGS
#endif

/*
** MR_ASM_FALLTHROUGH is executed when falling through into an entry point.
** It may call `goto MR_skip(label)' if it wishes to skip past the
** label and the MR_INLINE_ASM_FIXUP_REGS code.
*/
#ifndef MR_ASM_FALLTHROUGH
#define MR_ASM_FALLTHROUGH(label)
#endif

/*
** MR_INLINE_ASM_GLOBALIZE_LABEL is an assembler string to
** declare an entry label as global.  The following definition
** using `.globl' should work with the GNU assembler and
** with most Unix assemblers.
*/
#ifndef MR_INLINE_ASM_GLOBALIZE_LABEL
#define MR_INLINE_ASM_GLOBALIZE_LABEL(label) \
	"	.globl _entry_" MR_STRINGIFY(label) "\n"
#endif

/*
** MR_INLINE_ASM_ENTRY_LABEL_TYPE is an assembler string to
** declare the "type" of a label as function (i.e. code), not data,
** if this is needed.
*/
#ifndef MR_INLINE_ASM_ENTRY_LABEL_TYPE
#define MR_INLINE_ASM_ENTRY_LABEL_TYPE(label)
#endif

/*
** MR_INLINE_ASM_ENTRY_LABEL is an assembler string to
** define an assembler entry label.
*/
#ifndef MR_INLINE_ASM_ENTRY_LABEL
#define MR_INLINE_ASM_ENTRY_LABEL(label)	\
	"_entry_" MR_STRINGIFY(label) ":\n"
#endif

/*
** MR_ASM_JUMP is used to jump to an entry point defined with
** MR_ASM_ENTRY, MR_ASM_STATIC_ENTRY, or MR_ASM_LOCAL_ENTRY.
*/
#ifndef MR_ASM_JUMP
#define MR_ASM_JUMP(address)	goto *(address)
#endif

/*---------------------------------------------------------------------------*/

/* The code from here on should be architecture-independent. */

/*---------------------------------------------------------------------------*/

/*
** MR_ASM_ENTRY is used to declare an external entry point
** using a (global) assembler label.
*/
#define MR_ASM_ENTRY(label) 				\
	MR_ASM_FALLTHROUGH(label)			\
  	MR_entry(label):				\
	__asm__ __volatile__(				\
		MR_INLINE_ASM_GLOBALIZE_LABEL(label)	\
		MR_INLINE_ASM_ENTRY_LABEL_TYPE(label)	\
		MR_INLINE_ASM_ENTRY_LABEL(label)	\
		MR_INLINE_ASM_FIXUP_REGS		\
	);						\
	MR_skip(label): ;

/*
** MR_ASM_STATIC_ENTRY is the same as MR_ASM_ENTRY,
** except that its scope is local to a C file, rather than global.
** Note that even static entry points must do MR_INLINE_ASM_FIXUP_REGS,
** since although there won't be any direct calls to them from another
** C file, their address may be taken and so there may be indirect
** calls.
*/
#define MR_ASM_STATIC_ENTRY(label) 			\
	MR_ASM_FALLTHROUGH(label)			\
  	MR_entry(label):				\
	__asm__ __volatile__(				\
		MR_INLINE_ASM_ENTRY_LABEL_TYPE(label)	\
		MR_INLINE_ASM_ENTRY_LABEL(label)	\
		MR_INLINE_ASM_FIXUP_REGS		\
	);						\
	MR_skip(label): ;

/*
** MR_ASM_LOCAL_ENTRY is the same as MR_ASM_ENTRY,
** except that its scope is local to a MR_BEGIN_MODULE...MR_END_MODULE
** block, rather than being global.
** Note that even local entry points must do MR_INLINE_ASM_FIXUP_REGS, since
** although there won't be any direct calls to them from another
** C file, their address may be taken and so there may be indirect
** calls.
*/
#define MR_ASM_LOCAL_ENTRY(label) 			\
	MR_ASM_FALLTHROUGH(label)			\
  	MR_entry(label):				\
  	MR_ASM_FIXUP_REGS				\
	MR_skip(label): ;

/*---------------------------------------------------------------------------*/

#if defined(USE_GCC_NONLOCAL_GOTOS)

  #ifndef __GNUC__
  #error "You must use gcc if you define USE_GCC_NONLOCAL_GOTOS"
  #endif

  /* Define the type of a module initialization function */
  typedef void MR_ModuleFunc(void);

  /* The following macro expands to a dummy assembler statement which
  ** contains no code, but which tells gcc that it uses the specified
  ** address as an input value.  This is used to trick gcc into
  ** thinking that the address is used, in order to suppress unwanted
  ** optimizations.  (We used to use `volatile_global_pointer =
  ** address' to suppress optimization, but this way is better because
  ** it doesn't generate any code.)
  */
  #define MR_PRETEND_ADDRESS_IS_USED(address)		\
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
  ** That is what the two occurrences of the MR_PRETEND_ADDRESS_IS_USED
  ** macro are for.
  ** For versions of gcc later than egcs 1.1.2 (which corresponds to gcc 2.91,
  ** according to __GNUC_MINOR__), and in particular for gcc 2.95,
  ** we also need to include at least one `goto *' with an unknown
  ** target, so that gcc doesn't optimize away all the labels
  ** because it thinks they are unreachable.
  ** The MR_dummy_identify_function() function just returns the address
  ** passed to it, so `goto *MR_dummy_identify_function(&& dummy_label);
  ** dummy_label:' is the same as `goto dummy_label; label:', i.e. it just
  ** falls through. For older versions of gcc, we don't do this, since it
  ** adds significantly to the code size.
  */
  #if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ > 91)
    /* gcc version > egcs 1.1.2 */
    #define MR_BEGIN_MODULE(module_name)				\
	MR_MODULE_STATIC_OR_EXTERN void module_name(void);		\
	MR_MODULE_STATIC_OR_EXTERN void module_name(void) {		\
		MR_PRETEND_ADDRESS_IS_USED(module_name);		\
		MR_PRETEND_ADDRESS_IS_USED(				\
			&&MR_PASTE2(module_name,_dummy_label));		\
		goto *MR_dummy_identify_function(			\
			&&MR_PASTE2(module_name,_dummy_label));		\
		MR_PASTE2(module_name,_dummy_label):			\
		{
  #else /* gcc version <= egcs 1.1.2 */
    #define MR_BEGIN_MODULE(module_name)				\
	MR_MODULE_STATIC_OR_EXTERN void module_name(void);		\
	MR_MODULE_STATIC_OR_EXTERN void module_name(void) {		\
		MR_PRETEND_ADDRESS_IS_USED(module_name);		\
		MR_PRETEND_ADDRESS_IS_USED(				\
			&&MR_PASTE2(module_name,_dummy_label));		\
		MR_PASTE2(module_name,_dummy_label):			\
		{
  #endif /* gcc version <= egcs 1.1.2 */
  /* initialization code for module goes between MR_BEGIN_MODULE */
  /* and MR_BEGIN_CODE */
  #define MR_BEGIN_CODE } return; {
  /* body of module goes between MR_BEGIN_CODE and MR_END_MODULE */
  #define MR_END_MODULE } }


  #if defined(USE_ASM_LABELS)
    #define MR_declare_entry(label)		\
	extern void label(void) __asm__("_entry_" MR_STRINGIFY(label))
    #define MR_declare_static(label)		\
	static void label(void) __asm__("_entry_" MR_STRINGIFY(label))
    #define MR_define_extern_entry(label)	MR_declare_entry(label)
    #define MR_define_entry(label)		\
		MR_ASM_ENTRY(label)		\
	}					\
	label:					\
	MR_PRETEND_ADDRESS_IS_USED(&&MR_entry(label));	\
	{
    #define MR_define_static(label)		\
		MR_ASM_STATIC_ENTRY(label) 	\
	}					\
	label:					\
	MR_PRETEND_ADDRESS_IS_USED(&&MR_entry(label));	\
	{
    /*
    ** The MR_PRETEND_ADDRESS_IS_USED macro is necessary to 
    ** prevent an over-zealous gcc from optimizing away `label'
    ** and the code that followed. 
    */
    #define MR_init_entry(label)	\
	MR_PRETEND_ADDRESS_IS_USED(&&label); \
	MR_make_entry(MR_STRINGIFY(label), label, label)
    #define MR_init_entry_ai(label)	\
	MR_PRETEND_ADDRESS_IS_USED(&&label); \
	MR_make_entry_ai(MR_STRINGIFY(label), label, label)
    #define MR_init_entry_sl(label)	\
	MR_PRETEND_ADDRESS_IS_USED(&&label); \
	MR_make_entry_sl(MR_STRINGIFY(label), label, label)

    #define MR_ENTRY(label) 		(&label)
    #define MR_STATIC(label) 		(&label)

    #ifndef MR_JUMP
    #define MR_JUMP(label)		MR_ASM_JUMP(label)
    #endif

  #else
    /* !defined(USE_ASM_LABELS) */

    #define MR_declare_entry(label)	extern MR_Code * MR_entry(label)
    #define MR_declare_static(label)	static MR_Code * MR_entry(label)
    #define MR_define_extern_entry(label) MR_Code * MR_entry(label)
    #define MR_define_entry(label)	\
	}	\
	MR_entry(label): \
	label:	\
	{
    #define MR_define_static(label)	\
	}	\
	MR_entry(label): \
	label:	\
	{
    #define MR_init_entry(label)	\
	MR_make_entry(MR_STRINGIFY(label), &&label, label);	\
	MR_entry(label) = &&label
    #define MR_init_entry_ai(label)	\
	MR_make_entry_ai(MR_STRINGIFY(label), &&label, label);	\
	MR_entry(label) = &&label
    #define MR_init_entry_sl(label)	\
	MR_make_entry_sl(MR_STRINGIFY(label), &&label, label);	\
	MR_entry(label) = &&label
    #define MR_ENTRY(label) 		(MR_entry(label))
    #define MR_STATIC(label) 		(MR_entry(label))

    #ifndef MR_JUMP
    #define MR_JUMP(label)		goto *(label)
    #endif

  #endif /* !defined(USE_ASM_LABELS) */

  #define MR_declare_local(label)	/* no declaration required */
  #define MR_define_local(label)	\
  		MR_ASM_LOCAL_ENTRY(label)	\
	}	\
	label:	\
	{
  #define MR_init_local(label)	\
  	MR_make_local(MR_STRINGIFY(label), &&MR_entry(label), label)
  #define MR_init_local_ai(label)	\
  	MR_make_local_ai(MR_STRINGIFY(label), &&MR_entry(label), label)
  #define MR_init_local_sl(label)	\
  	MR_make_local_sl(MR_STRINGIFY(label), &&MR_entry(label), label)
  #define MR_define_label(label)	MR_define_local(label)
  #define MR_declare_label(label)	/* no declaration required */
  #define MR_init_label(label)	\
	MR_make_label(MR_STRINGIFY(label), &&MR_entry(label), label)
  #define MR_init_label_ai(label)	\
	MR_make_label_ai(MR_STRINGIFY(label), &&MR_entry(label), label)
  #define MR_init_label_sl(label)	\
	MR_make_label_sl(MR_STRINGIFY(label), &&MR_entry(label), label)

  #define MR_LOCAL(label)	(&&MR_entry(label))
  #define MR_LABEL(label)	(&&MR_entry(label))
  #define MR_GOTO(label)	do {					\
	  				MR_debuggoto(label);		\
					MR_JUMP(label);			\
  				} while(0)
  #define MR_GOTO_ENTRY(label) 	MR_GOTO(MR_ENTRY(label))
  #define MR_GOTO_STATIC(label)	MR_GOTO(MR_STATIC(label))
  #define MR_GOTO_LOCAL(label) 	MR_GOTO_LABEL(label)
  #define MR_GOTO_LABEL(label) 	do {					\
  					MR_debuggoto(MR_LABEL(label));	\
					goto label;			\
				} while(0)

  /*
  ** MR_GOTO_LABEL(label) is the same as MR_GOTO(MR_LABEL(label)) except
  ** that it may allow gcc to generate slightly better code
  */

#else
  /* !defined(USE_GCC_NONLOCAL_GOTOS) */

  /* Define the type of a module initialization function */
  typedef MR_Code * MR_ModuleFunc(void);

  #define MR_BEGIN_MODULE(module_name)	\
	MR_MODULE_STATIC_OR_EXTERN MR_Code* module_name(void); \
	MR_MODULE_STATIC_OR_EXTERN MR_Code* module_name(void) {
  #define MR_BEGIN_CODE			return 0;
  #define MR_END_MODULE			}

  #define MR_declare_entry(label)		extern MR_Code *label(void)
  #define MR_declare_static(label)		static MR_Code *label(void)
  #define MR_define_extern_entry(label)		MR_Code *label(void)
  #define MR_define_entry(label)		\
		MR_GOTO_LABEL(label);		\
	}					\
	MR_Code* label(void) {
  #define MR_define_static(label)		\
		MR_GOTO_LABEL(label);		\
	}					\
	static MR_Code* label(void) {
  #define MR_init_entry(label)		MR_make_entry(MR_STRINGIFY(label),    \
		  				label, label)
  #define MR_init_entry_ai(label)	MR_make_entry_ai(MR_STRINGIFY(label), \
		  				label, label)
  #define MR_init_entry_sl(label)	MR_make_entry_sl(MR_STRINGIFY(label), \
		  				label, label)

  #define MR_declare_local(label)	static MR_Code *label(void)
  #define MR_define_local(label)	\
		MR_GOTO_LABEL(label);	\
	}				\
	static MR_Code* label(void) {
  #define MR_init_local(label)		MR_make_local(MR_STRINGIFY(label),    \
		  				label, label)
  #define MR_init_local_ai(label)	MR_make_local_ai(MR_STRINGIFY(label), \
		  				label, label)
  #define MR_init_local_sl(label)	MR_make_local_sl(MR_STRINGIFY(label), \
		  				label, label)

  #define MR_declare_label(label)	static MR_Code *label(void)
  #define MR_define_label(label)	\
		MR_GOTO_LABEL(label);	\
	}				\
	static MR_Code* label(void) {
  #define MR_init_label(label)		MR_make_label(MR_STRINGIFY(label),    \
		  				label, label)
  #define MR_init_label_ai(label)	MR_make_label_ai(MR_STRINGIFY(label), \
		  				label, label)
  #define MR_init_label_sl(label)	MR_make_label_sl(MR_STRINGIFY(label), \
		  				label, label)

  #define MR_ENTRY(label) 	((MR_Code *) (label))
  #define MR_STATIC(label) 	((MR_Code *) (label))
  #define MR_LOCAL(label)	((MR_Code *) (label))
  #define MR_LABEL(label)	((MR_Code *) (label))
  /*
  ** The call to MR_debuggoto() is in the driver function in mercury_engine.c,
  ** which is why the following definitions have no MR_debuggoto().
  */
  #define MR_GOTO(label)	return (label)
  #define MR_GOTO_ENTRY(label) 	MR_GOTO(MR_ENTRY(label))
  #define MR_GOTO_STATIC(label) MR_GOTO(MR_STATIC(label))
  #define MR_GOTO_LOCAL(label) 	MR_GOTO(MR_LOCAL(label))
  #define MR_GOTO_LABEL(label) 	MR_GOTO(MR_LABEL(label))

#endif /* !defined(USE_GCC_NONLOCAL_GOTOS) */

/* definitions for computed gotos */

#define MR_COMPUTED_GOTO(val, labels) 			\
	{						\
		static MR_Code *jump_table[] = {	\
			labels				\
		};					\
	  	MR_GOTO(jump_table[val]);		\
	}
#define MR_AND ,	/* used to separate the labels */

#endif /* not MERCURY_GOTO_H */
