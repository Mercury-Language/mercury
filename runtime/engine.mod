#include	"imp.h"
#include 	<string.h>
#include 	<stdlib.h>
#include 	<stdio.h>
#include	"dummy.h"
#include	"init.h"

/* GLOBAL VARIABLES */

/* heap, stack, and choice point stack */

Word	heap[MAXHEAP];
Word	stack[MAXSTACK];
Word	cpstack[MAXCPSTACK];

Word	*heapmin    = &heap[0];
Word	*stackmin   = &stack[CACHE_OFFSET];
Word	*cpstackmin = &cpstack[CACHE_OFFSET*2];

/* statistics gathering */

#ifndef SPEED

Word	*heapmax;
Word	*stackmax;
Word	*cpstackmax;

#endif

/* debugging options */

bool	gotodebug    = FALSE;
bool	calldebug    = FALSE;
bool	heapdebug    = FALSE;
bool	stackdebug   = FALSE;
bool	cpstackdebug = FALSE;
bool	detaildebug  = FALSE;
bool	finaldebug   = FALSE;

/* label table */

int	cur_entry = 0;
Label	entries[MAXLABELS];

/* misc. */

char	scratchbuf[256];

/*---------------------------------------------------------------------------*/

#ifdef USE_GCC_NONLOCAL_GOTOS

#define LOCALS_SIZE 1024	/* amount of space to reserve for local vars */
#define MAGIC_MARKER 187	/* a random character */
#define MAGIC_MARKER_2 142	/* another random character */

void *global_pointer;		/* used to defeat possible optimization	*/

#endif

/*---------------------------------------------------------------------------*/

/* FUNCTION PROTOTYPES */

void init_engine(void);
void call_engine(Code *entry_point);
static void init_entries(void);
static void init_registers(void);
#ifndef USE_GCC_NONLOCAL_GOTOS
static Code * engine_done(void);
#endif

void special_labels_module(void);

extern EntryPoint ENTRY(do_not_reached);

/*---------------------------------------------------------------------------*/

void init_engine(void)
{
	init_entries();
	init_modules();
	init_registers();
#ifndef USE_GCC_NONLOCAL_GOTOS
	makelabel("engine_done", LABEL(engine_done));
#endif
}

/*
** initialize the virtual machine registers
*/
static void init_registers(void)
{
	hp = heapmin;					
	sp = stackmin;					
	maxcp = curcp = childcp = cpstackmin;		
							
	/* set up a buffer zone */			
	succip = ENTRY(do_not_reached);			
	mkcp("buffer_zone", 0, ENTRY(do_not_reached));		
	cpstackmin = maxcp;				

	save_registers();
}

/*
** initialize the table of entry points & labels
*/
static void init_entries(void)
{
	int i;
	for (i = 0; i < MAXLABELS; i++)
		entries[i].e_name = "NOT_IN_USE";
}

/*---------------------------------------------------------------------------*/

/*
** call_engine(Code *entry_point)
**
**	This routine calls a Prolog routine from C.
**	(Well, actually it calls the "C as portable assembler" code
**	for a Ptah/moded-Goedel/whatever-we-decide-to-call-it program,
**	but you get the idea.  I'm just using the word Prolog in a very
**	general sense.)
**
**	The called routine must be deterministic.
**	The virtual machine registers must be set up correctly
**	before the call.
**
**	The called routine may invoke C functions; currently this
**	is done by just invoking them directly, although that will
**	have to change if we start using the caller-save registers.
**
**	The called routine may invoke C functions which in turn
**	invoke call_engine() to invoke invoke Prolog routines (which
**	in turn invoke C functions which ... etc. ad infinitum.)
**
**	There are two different implementations of this, one for gcc,
**	and another portable version that works on standard ANSI C compilers.
*/

#ifdef USE_GCC_NONLOCAL_GOTOS

/* The gcc-specific version */

void call_engine(Code *entry_point)
{
	/*
	** Allocate some space for local variables in other
	** procedures. This used to be done by just calling
	** alloca(1024), but on the mips that just decrements the
	** stack pointer, whereas local variables are referenced
	** via the frame pointer, so it didn't work.
	** This technique should work and should also be relatively portable,
	** just so long as local variables and temporaries are allocated in
	** the same way in every function.
	*/
	unsigned char locals[LOCALS_SIZE];

#ifndef SPEED
	/* ensure that we only make the label once */
	static bool initialized = FALSE;
	if (!initialized)
	{
		makelabel("engine_done", LABEL(engine_done));
		initialized = TRUE;
	}
#endif

	/*
	** restore any registers that get clobbered by the C function
	** call mechanism
	*/
	restore_registers();

	/*
	** We save the address of the locals in a global pointer to make
	** sure that gcc can't optimize them away.
	*/
	global_pointer = locals;

#ifndef SPEED
	memset((void *)locals, MAGIC_MARKER, LOCALS_SIZE);
#endif
	debugmsg1("in `call_engine', locals at %p\n", locals);

	/*
	** Now just call the entry point
	*/
	call(entry_point, &&engine_done);

engine_done:
	/*
	** Save any registers which will get clobbered by the normal
	** C function call / return mechanism
	*/
	save_registers();

	/*
	** We need to ensure that there is at least one
	** real function call in call_engine(), because
	** otherwise gcc thinks that it doesn't need to
	** restore the caller-save registers (such as
	** the return address!) because it thinks call_engine() is
	** a leaf routine which doesn't call anything else,
	** and so it thinks that they won't have been clobbered.
	**
	** It might perhaps be cleaner or more robust to just longjmp() out.
	*/
	dummy_function_call();
	debugmsg1("in label `engine_done', locals at %p\n", locals);

#ifndef SPEED
	/*
	** Check how much of the space we reserved for local variables
	** was actually used.
	*/
	{
		int low = 0, high = LOCALS_SIZE;
		int used_low, used_high;

		while (low < high && locals[low] == MAGIC_MARKER)
			low++;
		while (low < high && locals[high - 1] == MAGIC_MARKER)
			high--;
		used_low = high;
		used_high = LOCALS_SIZE - low;
		printf("max locals used:  %3d bytes (probably)\n",
			min(high, LOCALS_SIZE - low));
		printf("(low mark = %d, high mark = %d)\n", low, high);
	}
#endif
}

#else /* not USE_GCC_NONLOCAL_GOTOS */

/*
** The portable version
**
** To keep the main dispatch loop tight, instead of returning a null
** pointer to indicate when we've finished executing, we just longjmp()
** out.
**
** With register windows, we need to restore the registers to
** their initialized values from their saved copies.
** This must be done in a function engine_init_registers() rather
** than directly from call_engine() because otherwise their value
** would get mucked up because of the function call from call_engine().
*/

#include <setjmp.h>
static jmp_buf *engine_jmp_buf;

static Code * engine_done(void)
{
	save_registers();

	debugmsg0("longjmping out...\n");
	longjmp(*engine_jmp_buf, 1);
}

static Code * engine_init_registers(void)
{
	restore_registers();
	succip = engine_done;
	return NULL;
}

void call_engine(Code *entry_point)
{
	typedef Code *Func(void);
	reg Func *fp;
	jmp_buf curr_jmp_buf;
	jmp_buf * volatile prev_jmp_buf;

	/*
	** Preserve the value of engine_jmp_buf on the C stack.
	** This is so "C calls Prolog which calls C which calls Prolog" etc.
	** will work.
	*/
	prev_jmp_buf = engine_jmp_buf;
	engine_jmp_buf = &curr_jmp_buf;

	/*
	** Mark this as the spot to return to.
	** On return, restore the saved value of engine_jmp_buf and then
	** exit.
	*/
	if (setjmp(curr_jmp_buf)) {
	    debugmsg0("...caught longjmp\n");
	    engine_jmp_buf = prev_jmp_buf;
	    return;
	}

	/*
	** Start up the actual engine.
	** The loop is unrolled a bit for efficiency.
	*/
	fp = engine_init_registers;
	fp = (*fp)();
	fp = entry_point;
	for(;;) {
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
	}
}
#endif /* not USE_GCC_NONLOCAL_GOTOS */

/*---------------------------------------------------------------------------*/

BEGIN_MODULE(special_labels_module)

BEGIN_CODE

do_redo:
	redo();

do_fail:
	fail();

do_reset_hp_fail:
	hp = recsavehp;
	fail();

do_reset_cpvar0_fail:
	hp = (Word *) cpvar(0);
	fail();

do_succeed:
	succeed();

do_slowneg_fail:
	/* XXX */

do_slowneg_succeed:
	/* XXX */

do_fastneg_redo:
	/* XXX */
	debugregs("arrived at fastneg_redo");
#ifndef SPEED
	dumpcpstack();
#endif
	maxcp = (Word *) pop();	/* prune intervening choice points */
	modcp((Code *) pop());
	redo();

do_fastneg_proceed:
	/* XXX */
	debugregs("arrived at fastneg_proceed");
	(void) pop();
	modcp((Code *) pop());
	proceed();

do_not_reached:
	printf("reached not_reached\n");
	exit(1);

END_MODULE
