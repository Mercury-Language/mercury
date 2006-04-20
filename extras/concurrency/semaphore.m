%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000-2001,2003-2004, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: semaphore.m.
% Main author: conway
% Stability: medium.
%
% This module implements a simple semaphore data type for allowing
% coroutines to synchronise with one another.
%
% The operations in this module are no-ops in the hlc grades which don't
% contain a .par component.
%
%-----------------------------------------------------------------------------%

:- module semaphore.
:- interface.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

:- type semaphore.

    % new(Sem, !IO) creates a new semaphore `Sem' with it's counter
    % initialized to 0.
    %
:- pred semaphore.new(semaphore::out, io::di, io::uo) is det.

    % wait(Sem, !IO) blocks until the counter associated with `Sem'
    % becomes greater than 0, whereupon it wakes, decrements the
    % counter and returns.
    %
:- pred semaphore.wait(semaphore::in, io::di, io::uo) is det.

    % try_wait(Sem, Succ, !IO) is the same as wait/3, except that
    % instead of blocking, it binds `Succ' to a boolean indicating
    % whether the call succeeded in obtaining the semaphore or not.
    %
:- pred semaphore.try_wait(semaphore::in, bool::out, io::di, io::uo) is det.

    % signal(Sem, !IO) increments the counter associated with `Sem'
    % and if the resulting counter has a value greater than 0, it wakes
    % one or more coroutines that are waiting on this semaphore (if
    % any).
    %
:- pred semaphore.signal(semaphore::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- pragma foreign_decl("C", "
    #include <stdio.h>
    #include ""mercury_context.h""
    #include ""mercury_thread.h""

    typedef struct ME_SEMAPHORE_STRUCT {
        int     count;
#ifndef MR_HIGHLEVEL_CODE
        MR_Context  *suspended;
#else
  #ifdef MR_THREAD_SAFE
        MercuryCond cond;
  #endif 
#endif
#ifdef MR_THREAD_SAFE
        MercuryLock lock;
#endif
    } ME_Semaphore;
").

:- pragma foreign_decl("C#", "
public class ME_Semaphore {
    public int count;
}
").

:- pragma foreign_type(c,  semaphore, "ME_Semaphore *").
:- pragma foreign_type(il, semaphore,
        "class [semaphore__csharp_code]ME_Semaphore").

:- pragma foreign_decl("C", "
#ifdef MR_CONSERVATIVE_GC
  void ME_finalize_semaphore(GC_PTR obj, GC_PTR cd);
#endif
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    new(Semaphore::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    MR_Word sem_mem;
    ME_Semaphore    *sem;

    MR_incr_hp(sem_mem,
        MR_round_up(sizeof(ME_Semaphore), sizeof(MR_Word)));
    sem = (ME_Semaphore *) sem_mem;
    sem->count = 0;
#ifndef MR_HIGHLEVEL_CODE
    sem->suspended = NULL;
#else
  #ifdef MR_THREAD_SAFE
    pthread_cond_init(&(sem->cond), MR_COND_ATTR);
  #endif
#endif
#ifdef MR_THREAD_SAFE
    pthread_mutex_init(&(sem->lock), MR_MUTEX_ATTR);
#endif

    /*
    ** The condvar and the mutex will need to be destroyed
    ** when the semaphore is garbage collected.
    */
#ifdef MR_CONSERVATIVE_GC
    GC_REGISTER_FINALIZER(sem, ME_finalize_semaphore, NULL, NULL, NULL);
#endif

    Semaphore = sem;
    IO = IO0;
").

:- pragma foreign_proc("C#",
    new(Semaphore::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    Semaphore = new ME_Semaphore();
    Semaphore.count = 0;
").

:- pragma foreign_code("C", "
#ifdef MR_CONSERVATIVE_GC
  void
  ME_finalize_semaphore(GC_PTR obj, GC_PTR cd)
  {
    ME_Semaphore    *sem;

    sem = (ME_Semaphore *) obj;

  #ifdef MR_THREAD_SAFE
    #ifdef MR_HIGHLEVEL_CODE
    pthread_cond_destroy(&(sem->cond));
    #endif
    pthread_mutex_destroy(&(sem->lock));
  #endif

    return;
  }
#endif
").

    % Because semaphore.signal has a local label, we may get
    % C compilation errors if inlining leads to multiple copies
    % of this code.
    % 
    % XXX get rid of this limitation at some stage.
    %
:- pragma no_inline(semaphore.signal/3).
:- pragma foreign_proc("C",
    signal(Semaphore::in, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    ME_Semaphore    *sem;
#ifndef MR_HIGHLEVEL_CODE
    MR_Context  *ctxt;
#endif

    sem = (ME_Semaphore *) Semaphore;

    MR_LOCK(&(sem->lock), ""semaphore__signal"");

#ifndef MR_HIGHLEVEL_CODE
    if (sem->count >= 0 && sem->suspended != NULL) {
        ctxt = sem->suspended;
        sem->suspended = ctxt->MR_ctxt_next;
        MR_UNLOCK(&(sem->lock), ""semaphore__signal"");
        MR_schedule(ctxt);
            /* yield() */
        MR_save_context(MR_ENGINE(MR_eng_this_context));
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
            &&signal_skip_to_the_end_1;
        MR_schedule(MR_ENGINE(MR_eng_this_context));
        MR_runnext();
signal_skip_to_the_end_1: ;
    } else {
        sem->count++;
        MR_UNLOCK(&(sem->lock), ""semaphore__signal"");
            /* yield() */
        MR_save_context(MR_ENGINE(MR_eng_this_context));
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
            &&signal_skip_to_the_end_2;
        MR_schedule(MR_ENGINE(MR_eng_this_context));
        MR_runnext();
signal_skip_to_the_end_2: ;
    }
#else
    sem->count++;
    MR_UNLOCK(&(sem->lock), ""semaphore__signal"");
    MR_SIGNAL(&(sem->cond));
#endif
    IO = IO0;
").

:- pragma foreign_proc("C#",
    signal(Semaphore::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    System.Threading.Monitor.Enter(Semaphore);
    Semaphore.count++;
        // XXX I think we only need to do a Pulse.
    System.Threading.Monitor.PulseAll(Semaphore);
    System.Threading.Monitor.Exit(Semaphore);
").

    % Because semaphore__wait has a local label, we may get
    % C compilation errors if inlining leads to multiple copies
    % of this code.
    % 
    % XXX get rid of this limitation at some stage.
    %
:- pragma no_inline(semaphore__wait/3).
:- pragma foreign_proc("C",
    wait(Semaphore::in, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    ME_Semaphore    *sem;
#ifndef MR_HIGHLEVEL_CODE
    MR_Context  *ctxt;
#endif

    sem = (ME_Semaphore *) Semaphore;

    MR_LOCK(&(sem->lock), ""semaphore__wait"");

#ifndef MR_HIGHLEVEL_CODE
    if (sem->count > 0) {
        sem->count--;
        MR_UNLOCK(&(sem->lock), ""semaphore__wait"");
    } else {
        MR_save_context(MR_ENGINE(MR_eng_this_context));
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
            &&wait_skip_to_the_end;
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_next = sem->suspended;
        sem->suspended = MR_ENGINE(MR_eng_this_context);
        MR_UNLOCK(&(sem->lock), ""semaphore__wait"");
        MR_runnext();
wait_skip_to_the_end: ;
    }
#else
    while (sem->count <= 0) {
        MR_WAIT(&(sem->cond), &(sem->lock));
    }

    sem->count--;

    MR_UNLOCK(&(sem->lock), ""semaphore__wait"");
#endif
    IO = IO0;
").

:- pragma foreign_proc("C#",
    wait(Semaphore::in, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    System.Threading.Monitor.Enter(Semaphore);

    while (Semaphore.count <= 0) {
        System.Threading.Monitor.Wait(Semaphore);
    }

    Semaphore.count--;

    System.Threading.Monitor.Exit(Semaphore);
").

semaphore.try_wait(Sem, Res, !IO) :-
    try_wait_2(Sem, Res0, !IO),
    Res = ( Res0 = 0 -> yes ; no ).

:- pred try_wait_2(semaphore::in, int::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    try_wait_2(Semaphore::in, Res::out, IO0::di, IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe],
"
    ME_Semaphore    *sem;

    sem = (ME_Semaphore *) Semaphore;

    MR_LOCK(&(sem->lock), ""semaphore.try_wait"");
    if (sem->count > 0) {
        sem->count--;
        MR_UNLOCK(&(sem->lock), ""semaphore.try_wait"");
        Res = 0;
    } else {
        MR_UNLOCK(&(sem->lock), ""semaphore.try_wait"");
        Res = 1;
    }
    IO = IO0;
").

:- pragma foreign_proc("C#",
        try_wait_2(Semaphore::in, Res::out, _IO0::di, _IO::uo),
        [promise_pure, will_not_call_mercury, thread_safe],
"
    if (System.Threading.Monitor.TryEnter(Semaphore)) {
        if (Semaphore.count > 0) {
            Semaphore.count--;
            System.Threading.Monitor.Exit(Semaphore);
            Res = 0;
        } else {
            System.Threading.Monitor.Exit(Semaphore);
            Res = 1;
        }
    } else {
        Res = 1;
    }
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
