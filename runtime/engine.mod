/*
** Copyright (C) 1995 University of Melbourne.
** This file may only be copied under the terms of the GNU Library General
** Public License - see the file COPYING.LIB in the Mercury distribution.
*/

#include	"imp.h"

#include	<stdio.h>
#include 	<string.h>
#include	<setjmp.h>

#include	"engine.h"

#include	"dummy.h"

#ifdef USE_GCC_NONLOCAL_GOTOS

#define LOCALS_SIZE	10024	/* amount of space to reserve for local vars */
#define MAGIC_MARKER	187	/* a random character */
#define MAGIC_MARKER_2	142	/* another random character */

#endif

static	void	call_engine_inner(Code *entry_point);

#ifndef USE_GCC_NONLOCAL_GOTOS
static	Code	*engine_done(void);
static	Code	*engine_init_registers(void);
#endif

bool	debugflag[MAXFLAG];

static jmp_buf *engine_jmp_buf;

/*
** init_engine() calls init_memory() which sets up all the necessary
** stuff for allocating memory-zones and other runtime areas (such as
** the zone structures and context structures). If PARALLEL is defined,
** this will cause the shared memory to be allocated.
** Next, init_engine() calls init_processes() which fork()s the right
** number of processes, and initializes the data structures for coordinating
** the interaction between multiple processes.
** Finally, init_engine() calls init_process_context() which initializes the
** local context for this process including the heap and solutions heap.
** If it is the original process, it allocates the initial context for main.
*/
void 
init_engine(void)
{
	init_memory();
	init_processes();
	init_process_context();

#ifndef USE_GCC_NONLOCAL_GOTOS
	make_label("engine_done", LABEL(engine_done));
#endif
	return;
}


/*
** start_mercury_engine(Code *entry_point)
**
** This routine is the top-level entry point into the Mercury runtime
** engine. It should only be called once.
**
** It invokes call_engine(entry_point) for the first process, and if there are
** other processes, they call call_engine(do_runnext) which makes them sleep
** until work becomes available.
*/

void 
start_mercury_engine(Code *entry_point)
{
	if (my_procnum == 0) {
		call_engine(entry_point);
	} else {
		call_engine(ENTRY(do_runnext));
	}

	return;
}

/*
** call_engine(Code *entry_point)
**
**	This routine calls a Mercury routine from C.
**
**	The called routine should be det/semidet/cc_multi/cc_nondet.
**	The virtual machine registers must be set up correctly
**	before the call.  Specifically, the non-transient real registers
**	must have valid values, and the fake_reg copies of the transient
**	(register window) registers must have valid values; call_engine()
**	will call restore_transient_registers() and will then assume that
**	all the registers have been correctly set up.
**
**	call_engine() will call save_registers() before returning.
**	That will copy the real registers we use to the fake_reg array.
**
**	Beware, however, that if you are planning to return to C code
**	that did not #include "regs.h" (directly or via e.g. "imp.h"),
**	and you have fiddled with the Mercury registers or invoked
**	call_engine() or anything like that, then you will need to
**	save the real registers that C is using before modifying the
**	Mercury registers and then restore them afterwards.
**
**	The called routine may invoke C functions; currently this
**	is done by just invoking them directly, although that will
**	have to change if we start using the caller-save registers.
**
**	The called routine may invoke C functions which in turn
**	invoke call_engine() to invoke invoke Mercury routines (which
**	in turn invoke C functions which ... etc. ad infinitum.)
**
**	call_engine() calls setjmp() and then invokes call_engine_inner()
**	which does the real work.  call_engine_inner() exits by calling
**	longjmp() to return to call_engine().  There are two 
**	different implementations of call_engine_inner(), one for gcc,
**	and another portable version that works on standard ANSI C compilers.
*/

void 
call_engine(Code *entry_point)
{

	jmp_buf		curr_jmp_buf;
	jmp_buf		* volatile prev_jmp_buf;

	/*
	** Preserve the value of engine_jmp_buf on the C stack.
	** This is so "C calls Mercury which calls C which calls Mercury" etc.
	** will work.
	*/

	prev_jmp_buf = engine_jmp_buf;
	engine_jmp_buf = &curr_jmp_buf;

	/*
	** Mark this as the spot to return to.
	** On return, restore the registers (since longjmp may clobber
	** them), restore the saved value of engine_jmp_buf, and then
	** exit.
	*/

	if (setjmp(curr_jmp_buf)) {
		debugmsg0("...caught longjmp\n");
		restore_registers();
		engine_jmp_buf = prev_jmp_buf;
		return;
	}

	call_engine_inner(entry_point);
}

#ifdef USE_GCC_NONLOCAL_GOTOS

/* The gcc-specific version */

void 
call_engine_inner(Code *entry_point)
{
	/*
	** Allocate some space for local variables in other
	** procedures. This is done because we may jump into the middle
	** of a C function, which may assume that space on the stack
	** has already beened allocated for its variables. Such space
	** would generally be used for expression temporary variables.
	** How did we arrive at the correct value of LOCALS_SIZE?
	** Good question. I think it's more voodoo than science.
	**
	** This used to be done by just calling
	** alloca(LOCALS_SIZE), but on the mips that just decrements the
	** stack pointer, whereas local variables are referenced
	** via the frame pointer, so it didn't work.
	** This technique should work and should be vaguely portable,
	** just so long as local variables and temporaries are allocated in
	** the same way in every function.
	*/

	unsigned char locals[LOCALS_SIZE];
{

#ifndef SPEED
{
	/* ensure that we only make the label once */
	static	bool	initialized = FALSE;

	if (!initialized)
	{
		make_label("engine_done", LABEL(engine_done));
		initialized = TRUE;
	}
}
#endif

	/*
	** restore any registers that get clobbered by the C function
	** call mechanism
	*/

	restore_transient_registers();

	/*
	** We save the address of the locals in a global pointer to make
	** sure that gcc can't optimize them away.
	*/

	global_pointer = locals;

#ifndef SPEED
	memset((void *)locals, MAGIC_MARKER, LOCALS_SIZE);
#endif
	debugmsg1("in `call_engine', locals at %p\n", (void *)locals);

	/*
	** Now just call the entry point
	*/

	noprof_call(entry_point, LABEL(engine_done));

Define_label(engine_done);
	/*
	** We need to ensure that there is at least one
	** real function call in call_engine(), because
	** otherwise gcc thinks that it doesn't need to
	** restore the caller-save registers (such as
	** the return address!) because it thinks call_engine() is
	** a leaf routine which doesn't call anything else,
	** and so it thinks that they won't have been clobbered.
	**
	** This probably isn't necessary now that we exit from this function
	** using longjmp(), but it doesn't do much harm, so I'm leaving it in.
	*/

	dummy_function_call();

	debugmsg1("in label `engine_done', locals at %p\n", locals);

#ifndef SPEED
	/*
	** Check how much of the space we reserved for local variables
	** was actually used.
	*/

	if (check_space) {
		int	low = 0, high = LOCALS_SIZE;
		int	used_low, used_high;

		while (low < high && locals[low] == MAGIC_MARKER) {
			low++;
		}
		while (low < high && locals[high - 1] == MAGIC_MARKER) {
			high--;
		}
		used_low = high;
		used_high = LOCALS_SIZE - low;
		printf("max locals used:  %3d bytes (probably)\n",
			min(high, LOCALS_SIZE - low));
		printf("(low mark = %d, high mark = %d)\n", low, high);
	}
#endif /* not SPEED */

	/*
	** Despite the above precautions with allocating a large chunk
	** of unused stack space, the return address may still have been
	** stored on the top of the stack, past our dummy locals,
	** where it may have been clobbered.
	** Hence the only safe way to exit is with longjmp().
	**
	** Since longjmp() may clobber the registers, we need to
	** save them first.
	*/
	save_registers();
	debugmsg0("longjmping out...\n");
	longjmp(*engine_jmp_buf, 1);
}} /* end call_engine_inner() */

/* with nonlocal gotos, we don't save the previous locations */
void 
dump_prev_locations(void) {}

#else /* not USE_GCC_NONLOCAL_GOTOS */

/*
** The portable version
**
** To keep the main dispatch loop tight, instead of returning a null
** pointer to indicate when we've finished executing, we just longjmp()
** out.  We need to save the registers before calling longjmp(),
** since doing a longjmp() might clobber them.
**
** With register windows, we need to restore the registers to
** their initialized values from their saved copies.
** This must be done in a function engine_init_registers() rather
** than directly from call_engine_inner() because otherwise their value
** would get mucked up because of the function call from call_engine_inner().
*/

static Code *
engine_done(void)
{
	save_registers();
	debugmsg0("longjmping out...\n");
	longjmp(*engine_jmp_buf, 1);
}

static Code *
engine_init_registers(void)
{
	restore_transient_registers();
	succip = engine_done;
	return NULL;
}

/*
** For debugging purposes, we keep a circular buffer of
** the last 40 locations that we jumped to.  This is
** very useful for determining the cause of a crash,
** since it runs a lot faster than -dg.
*/

#define NUM_PREV_FPS	40

typedef	void	(*FuncPtr)(void);
typedef Code	*Func(void);

static FuncPtr	prev_fps[NUM_PREV_FPS];
static int	prev_fp_index = 0;

void 
dump_prev_locations(void)
{
	int i, pos;

#if defined(SPEED) && !defined(DEBUG_GOTOS)
	if (tracedebug) 
#endif
	{
		printf("previous %d locations:\n", NUM_PREV_FPS);
		for (i = 0; i < NUM_PREV_FPS; i++) {
			pos = (i + prev_fp_index) % NUM_PREV_FPS;
			printlabel(prev_fps[pos]);
		}
	}
}

static void 
call_engine_inner(Code *entry_point)
{
	reg	Func	*fp;

	/*
	** Start up the actual engine.
	** The loop is unrolled a bit for efficiency.
	*/

	fp = engine_init_registers;
	fp = (*fp)();
	fp = entry_point;

#if defined(SPEED) && !defined(DEBUG_GOTOS)
if (!tracedebug) {
	for (;;)
	{
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
		fp = (*fp)();
	}
} else
#endif
	for (;;)
	{
		prev_fps[prev_fp_index] = (FuncPtr) fp;

		if (++prev_fp_index >= NUM_PREV_FPS)
			prev_fp_index = 0;

		debuggoto(fp);
		debugsreg();
		fp = (*fp)();
	}
} /* end call_engine_inner() */
#endif /* not USE_GCC_NONLOCAL_GOTOS */

BEGIN_MODULE(special_labels_module)

BEGIN_CODE

do_redo:
	redo();

do_fail:
	fail();

do_succeed:
	succeed();

do_last_succeed:
	succeed_discard();

do_not_reached:
	printf("reached not_reached\n");
	exit(1);
#ifndef	USE_GCC_NONLOCAL_GOTOS
	return 0;
#endif

END_MODULE
