/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

/* DEFINITIONS FOR THE "PORTABLE ASSEMBLER" NON-LOCAL GOTOS */

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

#if defined(USE_GCC_NONLOCAL_GOTOS)

  #ifndef __GNUC__
  #error "You must use gcc if you define USE_GCC_NONLOCAL_GOTOS"
  #endif

  #define BEGIN_MODULE(module_name)	\
	static void module_name(void) { {
  /* initialization code for module goes here */
  #define BEGIN_CODE } return; {
  /* body of module goes here */
  #define END_MODULE } }

  #if defined(USE_ASM_LABELS)
    #include	"dummy.h"

    #define Declare_entry(label)	\
	extern void label(void) __asm__("entry_" stringify(label))
    #define Declare_static(label)	\
	static void label(void) __asm__("entry_" stringify(label))
    #define Define_extern_entry(label)	Declare_entry(label)
    #define Define_entry(label)	\
	}	\
	label:	\
		__asm__(".globl entry_" stringify(label) "\n"	\
			"entry_" stringify(label) ":");	\
	{
    #define Define_static(label)	\
	}	\
	label:	\
		__asm__("entry_" stringify(label) ":");	\
	{
    /*
       The dummy asm statement below tells gcc that `&&label' gets used,
       thus preventing an over-zealous gcc from optimizing away `label'
       and the code that followed.  We used to use
       `volatile_global_pointer = &&label' to suppress optimization,
       but this way is better because it doesn't generate any code.
    */
    #define init_entry(label)	\
	__asm__("" :: "g"(&&label)); \
	make_entry(stringify(label), label)

    #define ENTRY(label) 	(&label)

  #else
    /* !defined(USE_ASM_LABELS) */

    #define Declare_entry(label)	extern Code * entry(label)
    #define Declare_static(label)	static Code * entry(label)
    #define Define_extern_entry(label)	Code * entry(label)
    #define Define_entry(label)	\
	}	\
	label:	\
	{
    #define Define_static(label)	\
	}	\
	label:	\
	{
    #define init_entry(label)	\
	make_entry(stringify(label), &&label);	\
	entry(label) = &&label
    #define ENTRY(label) 	(entry(label))

  #endif

  #define Declare_local(label)	/* no declaration required */
  #define Define_local(label)	\
	}	\
	label:	\
	{
  #define init_local(label)	make_local(stringify(label), &&label)
  #define Declare_label(label)	/* no declaration required */
  #define Define_label(label)	\
	}	\
	label:	\
	{
  #define init_label(label)	make_label(stringify(label), &&label)

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

  #define BEGIN_MODULE(module_name)	static Code* module_name(void) {
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
