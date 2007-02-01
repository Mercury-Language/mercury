%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001,2003-2004, 2006-2007 The University of Melbourne.
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
% The term `concurrency' here refers to threads, not necessarily to parallel
% execution.  (The latter is also possible if you are using one of the .par
% grades and the lowlevel C backend, e.g. grade asm_fast.par.gc).
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module thread.
:- interface.

:- import_module io.

:- include_module channel.
:- include_module mvar.
:- include_module semaphore.

%-----------------------------------------------------------------------------%

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

%-----------------------------------------------------------------------------%

    % XXX The following is necessary in order to avoid compilation errors in
    % lowlevel grades.  For some reason parent modules are not #including
    % their own .mh files.  Without this declaration the code for spawn/3
    % will not compile.  This can be removed once the problem with the .mh
    % files is fixed.
    %
:- pragma foreign_decl("C", "void ML_call_back_to_mercury_cc_multi(MR_Word);").

%-----------------------------------------------------------------------------%

:- pragma no_inline(spawn/3).
:- pragma foreign_proc("C",
    spawn(Goal::(pred(di, uo) is cc_multi), IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
#if !defined(MR_HIGHLEVEL_CODE)
    MR_Context  *ctxt;
    ctxt = MR_create_context(""spawn"", MR_CONTEXT_SIZE_REGULAR, NULL);
    ctxt->MR_ctxt_resume = &&spawn_call_back_to_mercury_cc_multi;
    
    /*
    ** Store the closure on the top of the new context's stack.
    */
    
    *(ctxt->MR_ctxt_sp) = Goal;
    ctxt->MR_ctxt_next = NULL;
    ctxt->MR_ctxt_thread_local_mutables =
        MR_clone_thread_local_mutables(MR_THREAD_LOCAL_MUTABLES);
    MR_schedule_context(ctxt);
    if (0) {
spawn_call_back_to_mercury_cc_multi:
        MR_save_registers();
        /*
        ** Get the closure from the top of the stack
        */
        ML_call_back_to_mercury_cc_multi(*((MR_Word *)MR_sp));
        MR_destroy_context(MR_ENGINE(MR_eng_this_context));
        MR_ENGINE(MR_eng_this_context) = NULL;
        MR_runnext();
    }
#else /* MR_HIGHLEVEL_CODE */

#if defined(MR_THREAD_SAFE)
    ML_create_thread(Goal);
#else
    MR_fatal_error(""spawn/3 requires a .par grade in high-level C grades."");
#endif

#endif /* MR_HIGHLEVEL_CODE */
    IO = IO0;
").

:- pragma foreign_proc("C#",
    spawn(Goal::(pred(di, uo) is cc_multi), _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    System.Threading.Thread t;
    MercuryThread mt = new MercuryThread(Goal);
    
    t = new System.Threading.Thread(
            new System.Threading.ThreadStart(mt.execute_goal));
    t.Start();
").

:- pragma no_inline(yield/2).
:- pragma foreign_proc("C",
    yield(IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
#ifndef MR_HIGHLEVEL_CODE
    MR_save_context(MR_ENGINE(MR_eng_this_context));
    MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
        &&yield_skip_to_the_end;
    MR_schedule_context(MR_ENGINE(MR_eng_this_context));
    MR_ENGINE(MR_eng_this_context) = NULL;
    MR_runnext();
yield_skip_to_the_end:
#endif
    IO = IO0;

").

yield(!IO).

:- pred call_back_to_mercury(pred(io, io), io, io).
:- mode call_back_to_mercury(pred(di, uo) is cc_multi, di, uo) is cc_multi.
:- pragma foreign_export("C",
    call_back_to_mercury(pred(di, uo) is cc_multi, di, uo),
    "ML_call_back_to_mercury_cc_multi").

call_back_to_mercury(Goal, !IO) :-
    Goal(!IO).

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

    /*
    ** We can't allocate `args' on the stack because this function may return
    ** before the child thread has got all the information it needs out of the
    ** structure.
    */
    args = MR_GC_NEW_UNCOLLECTABLE(ML_ThreadWrapperArgs);
    args->goal = goal;
    args->thread_local_mutables =
        MR_clone_thread_local_mutables(MR_THREAD_LOCAL_MUTABLES);

    if (pthread_create(&thread, MR_THREAD_ATTR, ML_thread_wrapper, args)) {
        MR_fatal_error(""Unable to create thread."");
    }

    /*
    ** XXX How do we ensure that the parent thread doesn't terminate until
    ** the child thread has finished it's processing?
    ** By the use of mutvars, or leave it up to user?
    */
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

    return NULL;
  }
#endif /* MR_HIGHLEVEL_CODE && MR_THREAD_SAFE */
").

:- pragma foreign_code("C#", "
public class MercuryThread {
    object[] Goal;

    public MercuryThread(object[] g)
    {
        Goal = g;
    }

    public void execute_goal()
    {
        mercury.thread.mercury_code.call_back_to_mercury_cc_multi(Goal);
    }
}").

%-----------------------------------------------------------------------------%
:- end_module thread.
%-----------------------------------------------------------------------------%
