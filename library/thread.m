%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001, 2003-2004, 2006-2008, 2010-2011, 2014 The
% University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: thread.m.
% Main author: conway.
% Stability: medium.
%
% This module defines the Mercury concurrency interface.
%
% The term `concurrency' refers to threads, not necessarily to parallel
% execution of those threads.  (The latter is also possible if you are using
% one of the .par grades or the Java or C# backends.)
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module thread.
:- interface.

:- import_module io.

:- include_module barrier.
:- include_module channel.
:- include_module mvar.
:- include_module semaphore.

%-----------------------------------------------------------------------------%

    % can_spawn succeeds if spawn/3 is supported in the current grade.
    %
:- pred can_spawn is semidet.

    % spawn(Closure, IO0, IO) is true iff `IO0' denotes a list of I/O
    % transactions that is an interleaving of those performed by `Closure'
    % and those contained in `IO' - the list of transactions performed by
    % the continuation of spawn/3.
    %
:- pred spawn(pred(io, io)::in(pred(di, uo) is cc_multi),
    io::di, io::uo) is cc_multi.

    % yield(IO0, IO) is logically equivalent to (IO = IO0) but
    % operationally, yields the Mercury engine to some other thread
    % if one exists.
    %
    % NOTE: this is not yet implemented in the hl*.par.gc grades; currently
    % it is a no-op in those grades.
    % 
:- pred yield(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
#ifndef MR_HIGHLEVEL_CODE
  #if (!defined(MR_EXEC_TRACE) && !defined(MR_DEEP_PROFILING)) || !defined(MR_USE_GCC_NONLOCAL_GOTOS)
    /*
    ** In calling thread.yield, semaphore.wait or semaphore.signal, the
    ** calling context may need to suspend and yield to another context.
    ** This is implemented by setting the resume address of the context to an
    ** auxiliary function outside of the foreign_proc.  This breaks when
    ** execution tracing or deep profiling are enabled as code inserted at the
    ** end of the foreign_proc won't be executed.  In those cases we rely on
    ** the gcc extension that allows us to take the address of labels within
    ** the foreign_proc, so the context will resume back inside the
    ** foreign_proc.
    **
    ** XXX implement those procedures as :- external procedures so that the
    ** transforms won't be applied
    */
    #define ML_THREAD_AVOID_LABEL_ADDRS
  #endif
#endif
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    can_spawn,
    [will_not_call_mercury, promise_pure, may_not_duplicate],
"
#if !defined(MR_HIGHLEVEL_CODE)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    #if defined(MR_THREAD_SAFE)
        SUCCESS_INDICATOR = MR_TRUE;
    #else
        SUCCESS_INDICATOR = MR_FALSE;
    #endif
#endif
").

:- pragma foreign_proc("C#",
    can_spawn,
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = true;
").

:- pragma foreign_proc("Java",
    can_spawn,
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = true;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    spawn(Goal::(pred(di, uo) is cc_multi), _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#if !defined(MR_HIGHLEVEL_CODE)
    MR_Context  *ctxt;

    MR_LOCK(&MR_thread_barrier_lock, ""thread.spawn"");
    MR_thread_barrier_count++;
    MR_UNLOCK(&MR_thread_barrier_lock, ""thread.spawn"");

    ctxt = MR_create_context(""spawn"", MR_CONTEXT_SIZE_REGULAR, NULL);
    ctxt->MR_ctxt_resume = MR_ENTRY(mercury__thread__spawn_begin_thread);
    
    /*
    ** Store the closure on the top of the new context's stack.
    */
    
    *(ctxt->MR_ctxt_sp) = Goal;
    ctxt->MR_ctxt_next = NULL;
    ctxt->MR_ctxt_thread_local_mutables =
        MR_clone_thread_local_mutables(MR_THREAD_LOCAL_MUTABLES);
    MR_schedule_context(ctxt);

#else /* MR_HIGHLEVEL_CODE */

#if defined(MR_THREAD_SAFE)
    ML_create_thread(Goal);
#else
    MR_fatal_error(""spawn/3 requires a .par grade in high-level C grades."");
#endif

#endif /* MR_HIGHLEVEL_CODE */
").

:- pragma foreign_proc("C#",
    spawn(Goal::(pred(di, uo) is cc_multi), _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    object[] thread_locals = runtime.ThreadLocalMutables.clone();
    MercuryThread mt = new MercuryThread(Goal, thread_locals);
    System.Threading.Thread thread = new System.Threading.Thread(
        new System.Threading.ThreadStart(mt.execute_goal));
    thread.Start();
").

:- pragma foreign_proc("Java",
    spawn(Goal::(pred(di, uo) is cc_multi), IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    MercuryThread mt = new MercuryThread((Object[]) Goal);
    Thread thread = new Thread(mt);
    thread.start();
    IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma no_inline(yield/2).
:- pragma foreign_proc("C",
    yield(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifndef MR_HIGHLEVEL_CODE
    MR_save_context(MR_ENGINE(MR_eng_this_context));
  #ifdef ML_THREAD_AVOID_LABEL_ADDRS
    MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
        MR_ENTRY(mercury__thread__yield_resume);
  #else
    MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
        &&yield_skip_to_the_end;
  #endif
    MR_schedule_context(MR_ENGINE(MR_eng_this_context));
    MR_ENGINE(MR_eng_this_context) = NULL;
    MR_idle();

  #ifndef ML_THREAD_AVOID_LABEL_ADDRS
    yield_skip_to_the_end:
  #endif
#endif
").

:- pragma foreign_proc("C#",
    yield(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    // Only available in .NET 4.0.
    // System.Threading.Yield();
").

:- pragma foreign_proc("Java",
    yield(IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        may_not_duplicate],
"
    java.lang.Thread.yield();
    IO = IO0;
").

yield(!IO).

%-----------------------------------------------------------------------------%
%
% Low-level C implementation
%

:- pragma foreign_decl("C",
"
/*
INIT mercury_sys_init_thread_modules
*/

#ifndef MR_HIGHLEVEL_CODE
    MR_define_extern_entry(mercury__thread__spawn_begin_thread);
    MR_declare_label(mercury__thread__spawn_end_thread);
    MR_define_extern_entry(mercury__thread__yield_resume);
#endif
").

:- pragma foreign_code("C",
"
#ifndef MR_HIGHLEVEL_CODE

    MR_declare_entry(mercury__do_call_closure_1);

    MR_BEGIN_MODULE(hand_written_thread_module)
        MR_init_entry_ai(mercury__thread__spawn_begin_thread);
        MR_init_label(mercury__thread__spawn_end_thread);
        MR_init_entry_ai(mercury__thread__yield_resume);
    MR_BEGIN_CODE

    MR_define_entry(mercury__thread__spawn_begin_thread);
    {
        /* Call the closure placed the top of the stack. */
        MR_r1 = *MR_sp;
        MR_noprof_call(MR_ENTRY(mercury__do_call_closure_1),
            MR_LABEL(mercury__thread__spawn_end_thread));
    }
    MR_define_label(mercury__thread__spawn_end_thread);
    {
        MR_LOCK(&MR_thread_barrier_lock, ""thread__spawn_end_thread"");
        MR_thread_barrier_count--;
        if (MR_thread_barrier_count == 0) {
            /*
            ** If this is the last spawned context to terminate and the
            ** main context was just waiting on us in order to terminate
            ** then reschedule the main context.
            */
            if (MR_thread_barrier_context) {
                MR_schedule_context(MR_thread_barrier_context);
                MR_thread_barrier_context = NULL;
            }
        }
        MR_UNLOCK(&MR_thread_barrier_lock, ""thread__spawn_end_thread"");

        MR_save_context(MR_ENGINE(MR_eng_this_context));
        MR_release_context(MR_ENGINE(MR_eng_this_context));
        MR_ENGINE(MR_eng_this_context) = NULL;
        MR_idle();
    }

    MR_define_entry(mercury__thread__yield_resume);
    {
        MR_proceed();
    }
    MR_END_MODULE

#endif

    /* forward decls to suppress gcc warnings */
    void mercury_sys_init_thread_modules_init(void);
    void mercury_sys_init_thread_modules_init_type_tables(void);
    #ifdef  MR_DEEP_PROFILING
    void mercury_sys_init_thread_modules_write_out_proc_statics(
        FILE *deep_fp, FILE *procrep_fp);
    #endif

    void mercury_sys_init_thread_modules_init(void)
    {
    #ifndef MR_HIGHLEVEL_CODE
        hand_written_thread_module();
    #endif
    }

    void mercury_sys_init_thread_modules_init_type_tables(void)
    {
        /* no types to register */
    }

    #ifdef  MR_DEEP_PROFILING
    void mercury_sys_init_thread_modules_write_out_proc_statics(FILE *deep_fp,
        FILE *procrep_fp)
    {
        /* no proc_statics to write out */
    }
    #endif
").

%-----------------------------------------------------------------------------%
%
% High-level C implementation
%

:- pragma foreign_decl("C", "
#if defined(MR_HIGHLEVEL_CODE) && defined(MR_THREAD_SAFE)
  #include  <pthread.h>

  int ML_create_thread(MR_Word goal);
  void *ML_thread_wrapper(void *arg);

  typedef struct ML_ThreadWrapperArgs ML_ThreadWrapperArgs;
  struct ML_ThreadWrapperArgs {
        MR_Word             goal;
        MR_ThreadLocalMuts  *thread_local_mutables;
  };
#endif /* MR_HIGHLEVEL_CODE && MR_THREAD_SAFE */
").

:- pragma foreign_code("C", "
#if defined(MR_HIGHLEVEL_CODE) && defined(MR_THREAD_SAFE)
  int ML_create_thread(MR_Word goal)
  {
    ML_ThreadWrapperArgs    *args;
    pthread_t               thread;
    pthread_attr_t          attrs;

    /*
    ** We can't allocate `args' on the stack because this function may return
    ** before the child thread has got all the information it needs out of the
    ** structure.
    */
    args = MR_GC_NEW_UNCOLLECTABLE(ML_ThreadWrapperArgs);
    args->goal = goal;
    args->thread_local_mutables =
        MR_clone_thread_local_mutables(MR_THREAD_LOCAL_MUTABLES);

    MR_LOCK(&MR_thread_barrier_lock, ""thread.spawn"");
    MR_thread_barrier_count++;
    MR_UNLOCK(&MR_thread_barrier_lock, ""thread.spawn"");

    pthread_attr_init(&attrs);
    pthread_attr_setdetachstate(&attrs, PTHREAD_CREATE_DETACHED);
    if (pthread_create(&thread, &attrs, ML_thread_wrapper, args)) {
        MR_fatal_error(""Unable to create thread."");
    }
    pthread_attr_destroy(&attrs);

    return MR_TRUE;
  }

  void *ML_thread_wrapper(void *arg)
  {
    ML_ThreadWrapperArgs    *args = arg;
    MR_Word                 goal;

    if (MR_init_thread(MR_use_now) == MR_FALSE) {
        MR_fatal_error(""Unable to init thread."");
    }

    MR_assert(MR_THREAD_LOCAL_MUTABLES == NULL);
    MR_SET_THREAD_LOCAL_MUTABLES(args->thread_local_mutables);

    goal = args->goal;
    MR_GC_free(args);

    ML_call_back_to_mercury_cc_multi(goal);

    MR_finalize_thread_engine();;

    MR_LOCK(&MR_thread_barrier_lock, ""ML_thread_wrapper"");
    MR_thread_barrier_count--;
    if (MR_thread_barrier_count == 0) {
        MR_SIGNAL(&MR_thread_barrier_cond, ""ML_thread_wrapper"");
    }
    MR_UNLOCK(&MR_thread_barrier_lock, ""ML_thread_wrapper"");

    return NULL;
  }
#endif /* MR_HIGHLEVEL_CODE && MR_THREAD_SAFE */
").

:- pred call_back_to_mercury(pred(io, io), io, io).
:- mode call_back_to_mercury(pred(di, uo) is cc_multi, di, uo) is cc_multi.
:- pragma foreign_export("C",
    call_back_to_mercury(pred(di, uo) is cc_multi, di, uo),
    "ML_call_back_to_mercury_cc_multi").
:- pragma foreign_export("IL",
    call_back_to_mercury(pred(di, uo) is cc_multi, di, uo),
    "ML_call_back_to_mercury_cc_multi").
:- pragma foreign_export("C#",
    call_back_to_mercury(pred(di, uo) is cc_multi, di, uo),
    "ML_call_back_to_mercury_cc_multi").
:- pragma foreign_export("Java",
    call_back_to_mercury(pred(di, uo) is cc_multi, di, uo),
    "ML_call_back_to_mercury_cc_multi").

call_back_to_mercury(Goal, !IO) :-
    Goal(!IO).

%-----------------------------------------------------------------------------%

:- pragma foreign_code("C#", "
public class MercuryThread {
    object[] Goal;
    object[] thread_local_mutables;

    public MercuryThread(object[] g, object[] tlmuts)
    {
        Goal = g;
        thread_local_mutables = tlmuts;
    }

    public void execute_goal()
    {
        runtime.ThreadLocalMutables.set_array(thread_local_mutables);
        thread.ML_call_back_to_mercury_cc_multi(Goal);
    }
}").

:- pragma foreign_code("Java", "
public static class MercuryThread implements Runnable {
    final Object[] Goal;

    public MercuryThread(Object[] g)
    {
        Goal = g;
    }

    public void run()
    {
        thread.ML_call_back_to_mercury_cc_multi(Goal);
    }
}").

%-----------------------------------------------------------------------------%
:- end_module thread.
%-----------------------------------------------------------------------------%
