%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001,2003-2004, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: spawn.m.
% Main author: conway.
% Stability: medium.
%
% This module provides `spawn/3' which is the primitive for starting the
% concurrent execution of a goal. The term `concurrent' here is referring
% to threads, not parallel execution, though the latter is possible by
% compiling in one of the *.par grades (e.g. asm_fast.gc.par or hlc.gc.par).
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module spawn.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

    % spawn(Closure, IO0, IO) is true iff IO0 denotes a list of I/O
    % transactions that is an interleaving of those performed by `Closure'
    % and those contained in IO - the list of transactions performed by
    % the continuation of spawn.
    %
:- pred spawn(pred(io, io), io, io).
:- mode spawn(pred(di, uo) is cc_multi, di, uo) is cc_multi.

    % yield(IO0, IO) is logically equivalent to (IO = IO0) but
    % operationally, yields the mercury engine to some other thread
    % if one exists.
    %
    % NOTE: this is not yet implemented in the hlc.par.gc grade.
    % 
:- pred yield(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "
#if defined(MR_HIGHLEVEL_CODE) && !defined(MR_THREAD_SAFE)
  #error The spawn module requires either hlc.par.gc grade or a non-hlc grade.
#endif

    #include <stdio.h>
").

:- pragma no_inline(spawn/3).
:- pragma foreign_proc("C",
    spawn(Goal::(pred(di, uo) is cc_multi), IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
#ifndef MR_HIGHLEVEL_CODE
    MR_Context  *ctxt;
    ctxt = MR_create_context(""spawn"", NULL);
    ctxt->MR_ctxt_resume = &&spawn_call_back_to_mercury_cc_multi;
        /* Store the closure on the top of the new context's stack. */
    *(ctxt->MR_ctxt_sp) = Goal;
    ctxt->MR_ctxt_next = NULL;
    MR_schedule(ctxt);
    if (0) {
spawn_call_back_to_mercury_cc_multi:
        MR_save_registers();
            /* Get the closure from the top of the stack */
        call_back_to_mercury_cc_multi(*((MR_Word *)MR_sp));
        MR_destroy_context(MR_ENGINE(MR_eng_this_context));
        MR_runnext();
    }
#else
    ME_create_thread(ME_thread_wrapper, (void *) Goal);
#endif
    IO = IO0;
").

:- pragma foreign_proc("C#",
    spawn(Goal::(pred(di, uo) is cc_multi), _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"{
    System.Threading.Thread t;
    MercuryThread mt = new MercuryThread(Goal);
    
    t = new System.Threading.Thread(
            new System.Threading.ThreadStart(mt.execute_goal));
    t.Start();
}").

:- pragma no_inline(yield/2).
:- pragma foreign_proc("C",
    yield(IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe], "{
        /* yield() */
#ifndef MR_HIGHLEVEL_CODE
    MR_save_context(MR_ENGINE(MR_eng_this_context));
    MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
        &&yield_skip_to_the_end;
    MR_schedule(MR_ENGINE(MR_eng_this_context));
    MR_runnext();
yield_skip_to_the_end:
#endif
    IO = IO0;

}").

yield(!IO).

:- pred call_back_to_mercury(pred(io, io), io, io).
:- mode call_back_to_mercury(pred(di, uo) is cc_multi, di, uo) is cc_multi.
:- pragma export(call_back_to_mercury(pred(di, uo) is cc_multi, di, uo),
        "call_back_to_mercury_cc_multi").

call_back_to_mercury(Goal, !IO) :-
    Goal(!IO).

:- pragma foreign_decl("C", "
#ifdef MR_HIGHLEVEL_CODE
  #include  <pthread.h>

  int ME_create_thread(void *(*func)(void *), void *arg);
  void *ME_thread_wrapper(void *arg);
#endif
").

:- pragma foreign_code("C", "
#ifdef MR_HIGHLEVEL_CODE
  int ME_create_thread(void *(*func)(void *), void *arg)
  {
    pthread_t   thread;

    if (pthread_create(&thread, MR_THREAD_ATTR, func, arg)) {
        MR_fatal_error(""Unable to create thread."");
    }

    /*
    ** XXX How do we ensure that the parent thread doesn't terminate until
    ** the child thread has finished it's processing?
    ** By the use of mutvars, or leave it up to user?
    */
    return MR_TRUE;
  }

  void *ME_thread_wrapper(void *arg)
  {
    call_back_to_mercury_cc_multi((MR_Word) arg);

    return NULL;
  }
#endif
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
        spawn.mercury_code.call_back_to_mercury_cc_multi(Goal);
    }
}
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
