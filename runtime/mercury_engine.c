// vim: ts=4 sw=4 expandtab ft=c

// Copyright (C) 1993-2001, 2003-2007, 2009-2011 The University of Melbourne.
// Copyright (C) 2014, 2016, 2018 The Mercury team.
// This file is distributed under the terms specified in COPYING.LIB.

/*
INIT mercury_sys_init_engine
ENDINIT
*/

#include    "mercury_imp.h"

#include    <stdio.h>
#include    <string.h>
#include    <setjmp.h>

#include    "mercury_engine.h"
#include    "mercury_memory_zones.h"    // for MR_create_zone()
#include    "mercury_memory_handlers.h" // for MR_default_handler()
#include    "mercury_threadscope.h"     // for event posting

#include    "mercury_dummy.h"

#ifndef MR_HIGHLEVEL_CODE

  #ifdef MR_USE_GCC_NONLOCAL_GOTOS

    // Space to reserve for local vars. If this parameter is modified
    // then the reference manual will also need to be updated.

    #define LOCALS_SIZE     10240

    #define MAGIC_MARKER    187     // a random character
    #define MAGIC_MARKER_2  142     // another random character

  #endif // MR_USE_GCC_NONLOCAL_GOTOS

  MR_NO_RETURN(static    void    call_engine_inner(MR_Code *entry_point));

  #ifndef MR_USE_GCC_NONLOCAL_GOTOS
    static MR_Code  *engine_done(void);
    static MR_Code  *engine_done_2(void);
    static MR_Code  *engine_init_registers(void);
  #endif

#endif // !MR_HIGHLEVEL_CODE

MR_bool MR_debugflag[MR_MAXFLAG];

MR_Debug_Flag_Info  MR_debug_flag_info[MR_MAXFLAG] = {
    { "prog",           MR_PROGFLAG },
    { "goto",           MR_GOTOFLAG },
    { "call",           MR_CALLFLAG },
    { "heap",           MR_HEAPFLAG },
    { "detstack",       MR_DETSTACKFLAG },
    { "nondetstack",    MR_NONDETSTACKFLAG },
    { "final",          MR_FINALFLAG },
    { "mem",            MR_MEMFLAG },
    { "sreg",           MR_SREGFLAG },
    { "trace",          MR_TRACEFLAG },
    { "table",          MR_TABLEFLAG },
    { "tablehash",      MR_TABLEHASHFLAG },
    { "tablestack",     MR_TABLESTACKFLAG },
    { "unbuf",          MR_UNBUFFLAG },
    { "agc",            MR_AGC_FLAG },
    { "ordreg",         MR_ORDINARY_REG_FLAG },
    { "anyreg",         MR_ANY_REG_FLAG },
    { "printlocn",      MR_PRINT_LOCN_FLAG },
    { "enabled",        MR_LLD_DEBUG_ENABLED_FLAG },
    { "notnearest",     MR_NOT_NEAREST_FLAG },
    { "debugslots",     MR_DEBUG_SLOTS_FLAG },
    { "deepdebugfile",  MR_DEEP_PROF_DEBUG_FILE_FLAG },
    { "stackextend",    MR_STACK_EXTEND_FLAG },
    { "detail",         MR_DETAILFLAG }
};

#ifndef MR_THREAD_SAFE
MercuryEngine MR_engine_base;
#endif

////////////////////////////////////////////////////////////////////////////

// MR_init_engine() calls MR_init_memory() which sets up all the necessary
// stuff for allocating memory-zones and other runtime areas (such as
// the zone structures and context structures).

void
MR_init_engine(MercuryEngine *eng)
{
    // First, ensure that the truly global stuff has been initialized
    // (if it was already initialized, this does nothing).

    MR_init_memory();

#if !defined(MR_USE_GCC_NONLOCAL_GOTOS) && !defined(MR_HIGHLEVEL_CODE)
    {
        static MR_bool made_engine_done_label = MR_FALSE;
        if (!made_engine_done_label) {
            MR_make_label("engine_done", MR_LABEL(engine_done), engine_done);
            made_engine_done_label = MR_TRUE;
        }
    }
#endif

    // Second, initialize the per-engine (i.e. normally per Posix thread)
    // stuff.

#ifndef MR_CONSERVATIVE_GC
    eng->MR_eng_heap_zone = MR_create_or_reuse_zone("heap",
        MR_heap_size, MR_next_offset(), MR_heap_zone_size, MR_default_handler);
    eng->MR_eng_hp = eng->MR_eng_heap_zone->MR_zone_min;

  #ifdef MR_NATIVE_GC
    eng->MR_eng_heap_zone2 = MR_create_or_reuse_zone("heap2",
        MR_heap_size, MR_next_offset(), MR_heap_zone_size, MR_default_handler);

    #ifdef MR_DEBUG_AGC_PRINT_VARS
    eng->MR_eng_debug_heap_zone = MR_create_or_reuse_zone("debug_heap",
        MR_debug_heap_size, MR_next_offset(),
        MR_debug_heap_zone_size, MR_default_handler);
    #endif
  #endif // MR_NATIVE_GC

  #ifdef MR_MIGHT_RECLAIM_HP_ON_FAILURE
    eng->MR_eng_solutions_heap_zone = MR_create_or_reuse_zone("solutions_heap",
        MR_solutions_heap_size, MR_next_offset(),
        MR_solutions_heap_zone_size, MR_default_handler);
    eng->MR_eng_sol_hp = eng->MR_eng_solutions_heap_zone->MR_zone_min;

    eng->MR_eng_global_heap_zone = MR_create_or_reuse_zone("global_heap",
        MR_global_heap_size, MR_next_offset(),
        MR_global_heap_zone_size, MR_default_handler);
    eng->MR_eng_global_hp = eng->MR_eng_global_heap_zone->MR_zone_min;
  #endif // MR_MIGHT_RECLAIM_HP_ON_FAILURE
#endif // !MR_CONSERVATIVE_GC

#ifdef  MR_THREAD_SAFE
    // The caller must initialise id and type.
    eng->MR_eng_id = MR_ENGINE_ID_NONE;
    eng->MR_eng_type = MR_ENGINE_TYPE_SHARED;
    eng->MR_eng_c_depth = 0;
#endif

#ifdef MR_LL_PARALLEL_CONJ
    eng->MR_eng_spark_deque = MR_GC_NEW(MR_SparkDeque);
    MR_init_wsdeque(eng->MR_eng_spark_deque, MR_INITIAL_SPARK_DEQUE_SIZE);
#endif

    // Don't allocate a context for this engine until it is actually needed.
    eng->MR_eng_this_context = NULL;
}

////////////////////////////////////////////////////////////////////////////

// The engine must be removed from MR_all_engine_bases BEFORE calling this
// function.

void MR_finalize_engine(MercuryEngine *eng)
{
    // XXX There are lots of other resources in MercuryEngine that
    // might need to be finalized.

    if (eng->MR_eng_this_context) {
        // Saving the context is very important before releasing it.
        // See the documentation for MR_release_context

        MR_save_context(eng->MR_eng_this_context);
        MR_release_context(eng->MR_eng_this_context);
    }

#if MR_THREADSCOPE
    if (eng->MR_eng_ts_buffer) {
        MR_threadscope_finalize_engine(eng);
    }
#endif
}

////////////////////////////////////////////////////////////////////////////

MercuryEngine *
MR_create_engine(void)
{
    MercuryEngine *eng;

    // We need to use MR_GC_NEW_UNCOLLECTABLE() here, rather than MR_GC_NEW(),
    // since the engine pointer will normally be stored in thread-local
    // storage, which is not traced by the conservative garbage collector.

    eng = MR_GC_NEW_UNCOLLECTABLE_ATTRIB(MercuryEngine, MR_ALLOC_SITE_RUNTIME);
    MR_init_engine(eng);
    return eng;
}

void
MR_destroy_engine(MercuryEngine *eng)
{
    MR_finalize_engine(eng);
    MR_GC_free_attrib(eng);
}

////////////////////////////////////////////////////////////////////////////

#ifdef MR_HIGHLEVEL_CODE

// This debugging hook is empty in the high-level code case:
// we don't save the previous locations.

void
MR_dump_prev_locations(void)
{
}

#else // !MR_HIGHLEVEL_CODE

// MR_Word *
// MR_call_engine(MR_Code *entry_point, MR_bool catch_exceptions)
//
// This routine calls a Mercury routine from C.
//
// The called routine should be det/semidet/cc_multi/cc_nondet.
//
// If the called routine returns normally (this includes the case of a
// semidet/cc_nondet routine failing, i.e. returning with
// MR_r1 = MR_FALSE), then MR_call_engine() will return NULL.
//
// If the called routine exits by throwing an exception, then the
// behaviour depends on the `catch_exceptions' flag.
// if `catch_exceptions' is true, then MR_call_engine() will return the
// Mercury exception object thrown. If `catch_exceptions' is false,
// then MR_call_engine() will not return; instead, the code for `throw'
// will unwind the stacks (including the C stack) back to the nearest
// enclosing exception handler.
//
// The virtual machine registers must be set up correctly before the call
// to MR_call_engine(). Specifically, the non-transient real registers
// must have valid values, and the fake_reg copies of the transient
// (register window) registers must have valid values; call_engine()
// will call MR_restore_transient_registers() and will then assume that
// all the registers have been correctly set up.
//
// call_engine() will call MR_save_registers() before returning.
// That will copy the real registers we use to the fake_reg array.
//
// Beware, however, that if you are planning to return to C code that did
// not #include "mercury_regs.h" (directly or via e.g. "mercury_imp.h"),
// and you have fiddled with the Mercury registers or invoked
// call_engine() or anything like that, then you will need to
// save the real registers that C is using before modifying the
// Mercury registers and then restore them afterwards.
//
// The called routine may invoke C functions; currently this
// is done by just invoking them directly, although that will
// have to change if we start using the caller-save registers.
//
// The called routine may invoke C functions which in turn
// invoke call_engine() to invoke invoke Mercury routines (which
// in turn invoke C functions which ... etc. ad infinitum.)
//
// MR_call_engine() calls setjmp() and then invokes call_engine_inner()
// which does the real work. call_engine_inner() exits by calling
// longjmp() to return to MR_call_engine(). There are two
// different implementations of call_engine_inner(), one for gcc,
// and another portable version that works on standard ANSI C compilers.

MR_Word *
MR_call_engine(MR_Code *entry_point, MR_bool catch_exceptions)
{

    jmp_buf     curr_jmp_buf;
    jmp_buf     * volatile prev_jmp_buf;
#if defined(MR_MPROF_PROFILE_TIME)
    MR_Code     * volatile prev_proc;
#endif

    // Preserve the value of MR_ENGINE(MR_eng_jmp_buf) on the C stack.
    // This is so "C calls Mercury which calls C which calls Mercury" etc.
    // will work.

    MR_restore_transient_registers();

    prev_jmp_buf = MR_ENGINE(MR_eng_jmp_buf);
    MR_ENGINE(MR_eng_jmp_buf) = &curr_jmp_buf;

    // Create an exception handler frame on the nondet stack so that
    // we can catch and return Mercury exceptions.

    if (catch_exceptions) {
        MR_create_exception_handler("call_engine", MR_C_LONGJMP_HANDLER, 0,
            MR_ENTRY(MR_do_fail));
    }

    // Mark this as the spot to return to.

#ifdef  MR_DEBUG_JMPBUFS
    printf("engine setjmp %p\n", curr_jmp_buf);
#endif

    if (setjmp(curr_jmp_buf)) {
        MR_Word * this_frame;
        MR_Word * exception;

#ifdef  MR_DEBUG_JMPBUFS
        printf("engine caught jmp %p %p\n",
            prev_jmp_buf, MR_ENGINE(MR_eng_jmp_buf));
#endif

        MR_debugmsg0("...caught longjmp\n");
        // On return, set MR_prof_current_proc to be the caller proc again
        // (if time profiling is enabled), restore the registers (since
        // longjmp may clobber them), and restore the saved value of
        // MR_ENGINE(MR_eng_jmp_buf).

        MR_update_prof_current_proc(prev_proc);
        MR_restore_registers();
        MR_ENGINE(MR_eng_jmp_buf) = prev_jmp_buf;
        if (catch_exceptions) {
            // Figure out whether or not we got an exception. If we got an
            // exception, then all of the necessary cleanup such as stack
            // unwinding has already been done, so all we have to do here
            // is to return the exception.

            exception = MR_ENGINE(MR_eng_exception);
            if (exception != NULL) {
                return exception;
            }
            // If we added an exception hander, but we didn't get an exception,
            // then we need to remove the exception handler frames from the
            // nondet stack and prune the trail ticket allocated by
            // MR_create_exception_handler().

            this_frame = MR_curfr;
            MR_maxfr_word = MR_prevfr_slot_word(this_frame);
            MR_curfr_word = MR_succfr_slot_word(this_frame);
#ifdef MR_USE_TRAIL
            MR_prune_ticket();
#endif
        }
        return NULL;
    }

    MR_ENGINE(MR_eng_jmp_buf) = &curr_jmp_buf;

    // If call profiling is enabled, and this is a case of Mercury calling C
    // code which then calls Mercury, then we record the (Mercury caller,
    // Mercury callee) pair in the table of call counts, if possible.

#ifdef MR_MPROF_PROFILE_CALLS
  #ifdef MR_MPROF_PROFILE_TIME
    if (MR_prof_current_proc != NULL) {
        MR_PROFILE(entry_point, MR_prof_current_proc);
    }
  #else
    // XXX There is not much we can do in this case to keep the call counts
    // accurate, since we don't know who the caller is.

  #endif
#endif // MR_MPROF_PROFILE_CALLS

    // If time profiling is enabled, then we need to save MR_prof_current_proc
    // so that we can restore it when we return. We must then set
    // MR_prof_current_proc to the procedure that we are about to call.
    //
    // We do this last thing before calling call_engine_inner(), since we want
    // to credit as much as possible of the time in C code to the caller,
    // not to the callee. Note that setting and restoring MR_prof_current_proc
    // here in call_engine() means that time in call_engine_inner()
    // unfortunately gets credited to the callee. That is not ideal, but we
    // can't move this code into call_engine_inner() since call_engine_inner()
    // can't have any local variables and this code needs the `prev_proc'
    // local variable.

#ifdef MR_MPROF_PROFILE_TIME
    prev_proc = MR_prof_current_proc;
    MR_set_prof_current_proc(entry_point);
#endif

    call_engine_inner(entry_point);
}

#ifdef MR_USE_GCC_NONLOCAL_GOTOS

// The gcc-specific version

static void
call_engine_inner(MR_Code *entry_point)
{
    // Allocate some space for local variables in other procedures. This is
    // done because we may jump into the middle of a C function, which may
    // assume that space on the stack has already been allocated for its
    // variables. Such space would generally be used for expression temporary
    // variables. How did we arrive at the correct value of LOCALS_SIZE?
    // Good question. I think it's more voodoo than science.
    //
    // This used to be done by just calling alloca(LOCALS_SIZE), but on MIPS
    // that just decrements the stack pointer, whereas local variables are
    // referenced via the frame pointer, so it didn't work. This technique
    // should work and should be vaguely portable, just so long as local
    // variables and temporaries are allocated in the same way in every
    // function.
    //
    // WARNING!
    // Do not add local variables to call_engine_inner that you expect
    // to remain live across Mercury execution - Mercury execution will
    // scribble on the stack frame for this function.

    unsigned char locals[LOCALS_SIZE];
{

#ifdef MR_LOWLEVEL_DEBUG
{
    // Ensure that we only make the label once.
    static  MR_bool initialized = MR_FALSE;

    if (!initialized) {
        MR_make_label("engine_done", MR_LABEL(engine_done), engine_done);
        MR_make_label("engine_done_2", MR_LABEL(engine_done_2), engine_done_2);
        initialized = MR_TRUE;
    }
}
#endif

    // Restore any registers that get clobbered by the C function call
    // mechanism.

    MR_restore_transient_registers();

    // We save the address of the locals in a global pointer to make sure that
    // gcc can't optimize them away.

    MR_global_pointer = locals;

#ifdef MR_LOWLEVEL_DEBUG
    MR_memset((void *) locals, MAGIC_MARKER, LOCALS_SIZE);
#endif
    MR_debugmsg1("in `call_engine_inner', locals at %p\n", (void *) locals);

    // We need to ensure that there is at least one real function call
    // in call_engine_inner(), because otherwise gcc thinks that it doesn't
    // need to restore the caller-save registers (such as the return address!)
    // because it thinks call_engine_inner() is a leaf routine which doesn't
    // call anything else, and so it thinks that they won't have been
    // clobbered.
    //
    // This probably isn't necessary now that we exit from this function
    // using longjmp(), but it doesn't do much harm, so I'm leaving it in.
    //
    // Also for gcc versions >= egcs1.1, we need to ensure that there is
    // at least one jump to an unknown label.

    goto *MR_dummy_identify_function(&&dummy_label);
dummy_label:

    // Increment the number of times we have entered this engine from C,
    // and push the current engine onto the context's stack of saved owners.

#ifdef  MR_THREAD_SAFE
    MR_ENGINE(MR_eng_c_depth)++;

    if (MR_ENGINE(MR_eng_this_context) != NULL) {
        MR_ResumeStack *elem;

        elem = MR_GC_NEW_ATTRIB(MR_ResumeStack, MR_ALLOC_SITE_RUNTIME);
        elem->MR_resume_engine = MR_ENGINE(MR_eng_id);
        elem->MR_resume_c_depth = MR_ENGINE(MR_eng_c_depth);
        elem->MR_resume_stack_next =
            MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume_stack;
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume_stack = elem;
    }
#endif

    // Now just call the entry point.

    MR_noprof_call(entry_point, MR_LABEL(engine_done));

MR_define_label(engine_done);

    assert(MR_ENGINE(MR_eng_this_context));

    // Check that we are reentering C in the correct engine.
    // If not, reschedule the context so that it will be picked up by
    // the correct engine when it is available.

#ifdef  MR_THREAD_SAFE
    {
        MR_Context      *this_ctxt;
        MR_ResumeStack  *elem;

        this_ctxt = MR_ENGINE(MR_eng_this_context);
        elem = this_ctxt->MR_ctxt_resume_stack;
        this_ctxt->MR_ctxt_resume_stack = elem->MR_resume_stack_next;

        if ((elem->MR_resume_engine == MR_ENGINE(MR_eng_id)) &&
            elem->MR_resume_c_depth == MR_ENGINE(MR_eng_c_depth))
        {
            MR_GC_free_attrib(elem);
            MR_GOTO_LABEL(engine_done_2);
        }

#ifdef MR_THREADSCOPE
        MR_threadscope_post_stop_context(MR_TS_STOP_REASON_YIELDING);
#endif
        MR_save_context(this_ctxt);
        this_ctxt->MR_ctxt_resume = MR_LABEL(engine_done_2);
        this_ctxt->MR_ctxt_resume_engine_required = MR_TRUE;
        this_ctxt->MR_ctxt_resume_engine = elem->MR_resume_engine;
        this_ctxt->MR_ctxt_resume_c_depth = elem->MR_resume_c_depth;
        MR_GC_free_attrib(elem);
        MR_schedule_context(this_ctxt);

        MR_ENGINE(MR_eng_this_context) = NULL;
        MR_idle();
    }
#endif

    // engine_done can be entered while the context is running on the wrong
    // engine (thread). If it turns out to be the case, then we suspend the
    // context and reschedule it so that it will resume in engine_done_2 and be
    // run on the correct engine. So engine_done_2 will always be run on the
    // engine which started the C->Mercury call, and engine_done ensures that
    // is the case.

MR_define_label(engine_done_2);

#ifdef MR_THREAD_SAFE
    // Decrement the number of times we have entered this engine from C.
    MR_ENGINE(MR_eng_c_depth)--;
#endif

    MR_debugmsg1("in label `engine_done', locals at %p\n", locals);

#ifdef MR_LOWLEVEL_DEBUG
    // Check how much of the space we reserved for local variables
    // was actually used.

    if (MR_check_space) {
        int low = 0, high = LOCALS_SIZE;
        int used_low, used_high;

        while (low < high && locals[low] == MAGIC_MARKER) {
            low++;
        }
        while (low < high && locals[high - 1] == MAGIC_MARKER) {
            high--;
        }
        used_low = high;
        used_high = LOCALS_SIZE - low;
        printf("max locals used:  %3d bytes (probably)\n",
            MR_min(high, LOCALS_SIZE - low));
        printf("(low mark = %d, high mark = %d)\n", low, high);
    }
#endif // MR_LOWLEVEL_DEBUG

    // Despite the above precautions with allocating a large chunk of unused
    // stack space, the return address may still have been stored on the
    // top of the stack, past our dummy locals, where it may have been
    // clobbered. Hence the only safe way to exit is with longjmp().
    //
    // Since longjmp() may clobber the registers, we need to save them first.

    MR_ENGINE(MR_eng_exception) = NULL;
    MR_save_registers();

#ifdef  MR_DEBUG_JMPBUFS
    printf("engine longjmp %p\n", MR_ENGINE(MR_eng_jmp_buf));
#endif

    MR_debugmsg0("longjmping out...\n");
    longjmp(*(MR_ENGINE(MR_eng_jmp_buf)), 1);
}} // end call_engine_inner()

// with nonlocal gotos, we don't save the previous locations
void
MR_dump_prev_locations(void)
{
}

#else // not MR_USE_GCC_NONLOCAL_GOTOS

// The portable version.
//
// To keep the main dispatch loop tight, instead of returning a null pointer
// to indicate when we have finished executing, we just longjmp() out.
// We need to save the registers before calling longjmp(), since doing
// a longjmp() might clobber them.
//
// With register windows, we need to restore the registers to their initialized
// values from their saved copies. This must be done in a function
// engine_init_registers() rather than directly from call_engine_inner()
// because otherwise their value would get mucked up because of the function
// call from call_engine_inner().
//
// XXX The portable version does not yet prevent Mercury code returning back
// into C code on the wrong Mercury engine. Therefore low-level .par grades
// without gcc non-local gotos are unsafe.

static MR_Code *
engine_done(void)
{
    MR_ENGINE(MR_eng_exception) = NULL;
    MR_save_registers();
    MR_debugmsg0("longjmping out...\n");
    longjmp(*(MR_ENGINE(MR_eng_jmp_buf)), 1);
    return NULL;    // Not executed, but required to suppress warnings.
}

static MR_Code *
engine_init_registers(void)
{
    MR_restore_transient_registers();
    MR_succip_word = (MR_Word) (MR_Code *) engine_done;
    return NULL;
}

// For debugging purposes, we keep a circular buffer of the last 40 locations
// that we jumped to. This is very useful for determining the cause of a
// crash, since it runs a lot faster than -dg.

#define NUM_PREV_FPS    40

typedef MR_Code *Func(void);

static MR_Code  *prev_fps[NUM_PREV_FPS];
static int      prev_fp_index = 0;

void
MR_dump_prev_locations(void)
{
    int i;
    int pos;

#if !defined(MR_DEBUG_GOTOS)
    if (MR_tracedebug)
#endif
    {
        printf("previous %d locations:\n", NUM_PREV_FPS);
        for (i = 0; i < NUM_PREV_FPS; i++) {
            pos = (i + prev_fp_index) % NUM_PREV_FPS;
            MR_printlabel(stdout, prev_fps[pos]);
        }
    }
}

static void
call_engine_inner(MR_Code *entry_point)
{
    register Func   *fp;

    // Start up the actual engine.
    // The loop is unrolled a bit for efficiency.

    fp = engine_init_registers;
    fp = (Func *) (*fp)();
    fp = (Func *) entry_point;

#if !defined(MR_DEBUG_GOTOS)
if (!MR_tracedebug) {
    for (;;) {
        fp = (Func *) (*fp)();
        fp = (Func *) (*fp)();
        fp = (Func *) (*fp)();
        fp = (Func *) (*fp)();
        fp = (Func *) (*fp)();
        fp = (Func *) (*fp)();
        fp = (Func *) (*fp)();
        fp = (Func *) (*fp)();
    }
} else
#endif
    for (;;) {
        prev_fps[prev_fp_index] = (MR_Code *) fp;

        if (++prev_fp_index >= NUM_PREV_FPS) {
            prev_fp_index = 0;
        }

        MR_debuggoto(fp);
        MR_debugsreg();
        fp = (Func *) (*fp)();
    }
} // end call_engine_inner()
#endif // not MR_USE_GCC_NONLOCAL_GOTOS

#endif // !MR_HIGHLEVEL_CODE

////////////////////////////////////////////////////////////////////////////

void
MR_terminate_engine(void)
{
    // We don't bother to deallocate memory...
    // that will happen automatically on process exit anyway.

}

////////////////////////////////////////////////////////////////////////////

#ifndef MR_HIGHLEVEL_CODE

MR_define_extern_entry(MR_do_redo);
MR_define_extern_entry(MR_do_fail);
MR_define_extern_entry(MR_do_succeed);
MR_define_extern_entry(MR_do_last_succeed);
MR_define_extern_entry(MR_do_not_reached);
MR_define_extern_entry(MR_exception_handler_do_fail);

MR_BEGIN_MODULE(special_labels_module)
    MR_init_entry_an(MR_do_redo);
    MR_init_entry_an(MR_do_fail);
    MR_init_entry_an(MR_do_succeed);
    MR_init_entry_an(MR_do_last_succeed);
    MR_init_entry_an(MR_do_not_reached);
    MR_init_entry_an(MR_exception_handler_do_fail);
MR_BEGIN_CODE

MR_define_entry(MR_do_redo);
    MR_redo();

MR_define_entry(MR_do_fail);
    MR_fail();

MR_define_entry(MR_do_succeed);
    MR_succeed();

MR_define_entry(MR_do_last_succeed);
    MR_succeed_discard();

MR_define_entry(MR_do_not_reached);
    MR_fatal_error("reached not_reached\n");

MR_define_entry(MR_exception_handler_do_fail);
    // `MR_exception_handler_do_fail' is the same as `MR_do_fail':
    // it just invokes MR_fail(). The reason we don't just use `MR_do_fail'
    // for this is that when unwinding the stack we check for a redoip
    // of `MR_exception_handler_do_fail' and handle it specially.

    MR_fail();

MR_END_MODULE

#endif // !MR_HIGHLEVEL_CODE

// Forward decls to suppress gcc warnings.
void mercury_sys_init_engine_init(void);
void mercury_sys_init_engine_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_engine_write_out_proc_statics(FILE *fp);
#endif

void mercury_sys_init_engine_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    special_labels_module();
#endif
}

void mercury_sys_init_engine_init_type_tables(void)
{
    // No types to register.
}

#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_engine_write_out_proc_statics(FILE *fp)
{
    // No proc_statics to write out.
}
#endif

////////////////////////////////////////////////////////////////////////////
