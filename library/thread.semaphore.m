%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2001,2003-2004, 2006-2007, 2009-2011 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury Team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: thread.semaphore.m.
% Main author: conway.
% Stability: medium.
%
% This module implements a simple semaphore data type for allowing
% threads to synchronise with one another.
%
% The operations in this module are no-ops in the hlc grades that do not
% contain a .par component.
%
%---------------------------------------------------------------------------%

:- module thread.semaphore.
:- interface.

:- import_module bool.
:- import_module io.

%---------------------------------------------------------------------------%

:- type semaphore.

    % init(Count, Sem, !IO) creates a new semaphore `Sem' with its counter
    % initialized to `Count'.
    %
:- pred init(int::in, semaphore::uo, io::di, io::uo) is det.

    % init(Sem, !IO) creates a new semaphore `Sem' with its counter
    % initialized to 0.
    %
:- pred init(semaphore::uo, io::di, io::uo) is det.

    % Sem = init(Count) returns a new semaphore `Sem' with its counter
    % initialized to `Count'.
    %
:- impure func impure_init(int::in) = (semaphore::uo) is det.

    % Sem = init(Count) returns a new semaphore `Sem' with its counter
    % initialized to `Count'.
    %
    % This has been renamed to impure_init.
    %
:- impure func init(int::in) = (semaphore::uo) is det.
:- pragma obsolete(init/1).

    % signal(Sem, !IO) increments the counter associated with `Sem'
    % and if the resulting counter has a value greater than 0, it wakes
    % one or more threads that are waiting on this semaphore (if
    % any).
    %
:- pred signal(semaphore::in, io::di, io::uo) is det.

    % wait(Sem, !IO) blocks until the counter associated with `Sem'
    % becomes greater than 0, whereupon it wakes, decrements the
    % counter and returns.
    %
:- pred wait(semaphore::in, io::di, io::uo) is det.

    % try_wait(Sem, Succ, !IO) is the same as wait/3, except that
    % instead of blocking, it binds `Succ' to a boolean indicating
    % whether the call succeeded in obtaining the semaphore or not.
    %
:- pred try_wait(semaphore::in, bool::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

init(Count, Semaphore, !IO) :-
    promise_pure (
        impure impure_init(Count, Semaphore)
    ).

init(Semaphore, !IO) :-
    init(0, Semaphore, !IO).

impure_init(Count) = Semaphore :-
    impure impure_init(Count, Semaphore).

init(Count) = Semaphore :-
    impure impure_init(Count, Semaphore).

signal(Semaphore, !IO) :-
    promise_pure (
        impure impure_signal(Semaphore),
        !:IO = !.IO
    ).

wait(Semaphore, !IO) :-
    promise_pure (
        impure impure_wait(Semaphore),
        !:IO = !.IO
    ).

try_wait(Sem, Res, !IO) :-
    promise_pure (
        impure impure_try_wait(Sem, Res)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- interface.

% The semaphore operations above can be used without the I/O state in impure
% code.  These predicates are provided for use by implementors.

:- impure pred impure_init(int::in, semaphore::uo) is det.

:- impure pred impure_init(semaphore::uo) is det.

:- impure pred impure_wait(semaphore::in) is det.

:- impure pred impure_try_wait(semaphore::in, bool::out) is det.

:- impure pred impure_signal(semaphore::in) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- implementation.

:- pragma foreign_decl("C", "
    #include <stdio.h>
    #include ""mercury_context.h""
    #include ""mercury_thread.h""

    typedef struct ML_SEMAPHORE_STRUCT {
        MR_Integer  count;
#ifndef MR_HIGHLEVEL_CODE
        MR_Context  *suspended_head;
        MR_Context  *suspended_tail;
#else
  #ifdef MR_THREAD_SAFE
        MercuryCond cond;
  #endif
#endif
#ifdef MR_THREAD_SAFE
        MercuryLock lock;
#endif
    } ML_Semaphore;
").

:- pragma foreign_code("C#", "
public class ML_Semaphore {
    public int count;
};
").

    % XXX the struct tag works around bug #19 in high-level C grades
:- pragma foreign_type("C",  semaphore, "struct ML_SEMAPHORE_STRUCT *",
    [can_pass_as_mercury_type]).
:- pragma foreign_type("C#", semaphore, "thread__semaphore.ML_Semaphore").
:- pragma foreign_type("Erlang", semaphore, "").
:- pragma foreign_type("Java", semaphore, "jmercury.runtime.Semaphore").

:- pragma foreign_decl("C", "
extern void
ML_finalize_semaphore(void *obj, void *cd);
").

%---------------------------------------------------------------------------%

impure_init(Semaphore) :-
    impure impure_init(0, Semaphore).

:- pragma foreign_proc("C",
    impure_init(Count::in, Semaphore::uo),
    [will_not_call_mercury, thread_safe],
"
    ML_Semaphore    *sem;

    MR_incr_hp_type_msg(sem, ML_Semaphore,
        MR_ALLOC_ID, ""thread.semaphore.semaphore/0"");
    sem->count = Count;
#ifndef MR_HIGHLEVEL_CODE
    sem->suspended_head = NULL;
    sem->suspended_tail = NULL;
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
    MR_GC_register_finalizer(sem, ML_finalize_semaphore, NULL);

    Semaphore = sem;
").

:- pragma foreign_proc("C#",
    impure_init(Count::in, Semaphore::uo),
    [will_not_call_mercury, thread_safe],
"
    Semaphore = new thread__semaphore.ML_Semaphore();
    Semaphore.count = Count;
").

:- pragma foreign_proc("Java",
    impure_init(Count::in, Semaphore::uo),
    [will_not_call_mercury, thread_safe],
"
    Semaphore = new jmercury.runtime.Semaphore(Count);
").

:- pragma foreign_code("C", "

void
ML_finalize_semaphore(void *obj, void *cd)
{
    ML_Semaphore    *sem;

    sem = (ML_Semaphore *) obj;

    #if defined(MR_THREAD_SAFE)
        #if defined(MR_HIGHLEVEL_CODE)
            pthread_cond_destroy(&(sem->cond));
        #endif
        pthread_mutex_destroy(&(sem->lock));
    #endif
}
").

%---------------------------------------------------------------------------%

    % impure_signal causes the calling context to resume in semaphore.nop,
    % which simply jumps to the succip. That will return control to the caller
    % of semaphore.signal as intended, but not if this procedure is inlined.
    %
    % XXX get rid of this limitation at some stage.
    %
:- pragma no_inline(semaphore.impure_signal/1).
:- pragma foreign_proc("C",
    impure_signal(Semaphore::in),
    [will_not_call_mercury, thread_safe],
"
    ML_Semaphore    *sem;
#ifndef MR_HIGHLEVEL_CODE
    MR_Context  *ctxt;
#endif

    sem = (ML_Semaphore *) Semaphore;

    MR_LOCK(&(sem->lock), ""semaphore.signal"");

#ifndef MR_HIGHLEVEL_CODE
    if (sem->count >= 0 && sem->suspended_head != NULL) {
        /* Reschedule the context at the start of the queue. */
        ctxt = sem->suspended_head;
        sem->suspended_head = ctxt->MR_ctxt_next;
        if (sem->suspended_tail == ctxt) {
            sem->suspended_tail = ctxt->MR_ctxt_next;
            assert(sem->suspended_tail == NULL);
        }
        MR_UNLOCK(&(sem->lock), ""semaphore.signal"");
        MR_schedule_context(ctxt);

        /* yield() */
        /* This context switch can be done more directly and faster */
        MR_save_context(MR_ENGINE(MR_eng_this_context));
      #ifdef ML_THREAD_AVOID_LABEL_ADDRS
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
            MR_ENTRY(mercury__thread__semaphore__nop);
      #else
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
            &&signal_skip_to_the_end_1;
      #endif
        MR_schedule_context(MR_ENGINE(MR_eng_this_context));

        MR_ENGINE(MR_eng_this_context) = NULL;
        MR_idle();

      #ifndef ML_THREAD_AVOID_LABEL_ADDRS
        signal_skip_to_the_end_1: ;
      #endif
    } else {
        sem->count++;
        MR_UNLOCK(&(sem->lock), ""semaphore.signal"");

        /* yield() */
        MR_save_context(MR_ENGINE(MR_eng_this_context));
      #ifdef ML_THREAD_AVOID_LABEL_ADDRS
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
            MR_ENTRY(mercury__thread__semaphore__nop);
      #else
        MR_ENGINE(MR_eng_this_context)->MR_ctxt_resume =
            &&signal_skip_to_the_end_2;
      #endif
        MR_schedule_context(MR_ENGINE(MR_eng_this_context));

        MR_ENGINE(MR_eng_this_context) = NULL;
        MR_idle();

      #ifndef ML_THREAD_AVOID_LABEL_ADDRS
        signal_skip_to_the_end_2: ;
      #endif
    }
#else
    sem->count++;
    MR_COND_SIGNAL(&(sem->cond), ""semaphore.signal"");
    MR_UNLOCK(&(sem->lock), ""semaphore.signal"");
#endif
").

:- pragma foreign_proc("C#",
    impure_signal(Semaphore::in),
    [will_not_call_mercury, thread_safe],
"
    System.Threading.Monitor.Enter(Semaphore);
    Semaphore.count++;
        // XXX I think we only need to do a Pulse.
    System.Threading.Monitor.PulseAll(Semaphore);
    System.Threading.Monitor.Exit(Semaphore);
").

:- pragma foreign_proc("Java",
    impure_signal(Semaphore::in),
    [will_not_call_mercury, thread_safe],
"
    Semaphore.release();
").

%---------------------------------------------------------------------------%

    % impure_wait causes the calling context to resume in semaphore.nop,
    % which simply jumps to the succip. That will return control to the caller
    % of semaphore.wait as intended, but not if this procedure is inlined.
    %
    % XXX get rid of this limitation at some stage.
    %
:- pragma no_inline(impure_wait/1).
:- pragma foreign_proc("C",
    impure_wait(Semaphore::in),
    [will_not_call_mercury, thread_safe],
"
    ML_Semaphore    *sem;
#ifndef MR_HIGHLEVEL_CODE
    MR_Context  *ctxt;
#endif

    sem = (ML_Semaphore *) Semaphore;

    MR_LOCK(&(sem->lock), ""semaphore.wait"");

#ifndef MR_HIGHLEVEL_CODE
    if (sem->count > 0) {
        sem->count--;
        MR_UNLOCK(&(sem->lock), ""semaphore.wait"");
    } else {
        MR_save_context(MR_ENGINE(MR_eng_this_context));

        /* Put the current context at the end of the queue. */
        ctxt = MR_ENGINE(MR_eng_this_context);
      #ifdef ML_THREAD_AVOID_LABEL_ADDRS
        ctxt->MR_ctxt_resume = MR_ENTRY(mercury__thread__semaphore__nop);
      #else
        ctxt->MR_ctxt_resume = &&wait_skip_to_the_end;
      #endif
        ctxt->MR_ctxt_next = NULL;
        if (sem->suspended_tail) {
            sem->suspended_tail->MR_ctxt_next = ctxt;
            sem->suspended_tail = ctxt;
        } else {
            sem->suspended_head = ctxt;
            sem->suspended_tail = ctxt;
        }
        MR_UNLOCK(&(sem->lock), ""semaphore.wait"");

        /* Make the current engine do something else. */
        MR_ENGINE(MR_eng_this_context) = NULL;
        MR_idle();

      #ifndef ML_THREAD_AVOID_LABEL_ADDRS
        wait_skip_to_the_end: ;
      #endif
    }
#else
    while (sem->count <= 0) {
        /*
        ** Although it goes against the spec, pthread_cond_wait() can
        ** return prematurely with the error code EINTR in glibc 2.3.2
        ** if the thread is sent a signal.
        */
        while (MR_COND_WAIT(&(sem->cond), &(sem->lock), ""semaphore.wait"")
            != 0)
        {
            /* do nothing */
        }
    }

    sem->count--;

    MR_UNLOCK(&(sem->lock), ""semaphore.wait"");
#endif
").

:- pragma foreign_proc("C#",
    impure_wait(Semaphore::in),
    [will_not_call_mercury, thread_safe],
"
    System.Threading.Monitor.Enter(Semaphore);

    while (Semaphore.count <= 0) {
        System.Threading.Monitor.Wait(Semaphore);
    }

    Semaphore.count--;

    System.Threading.Monitor.Exit(Semaphore);
").

:- pragma foreign_proc("Java",
    impure_wait(Semaphore::in),
    [will_not_call_mercury, thread_safe],
"
    /*
    ** acquire() might be useful as well; it will throw an exception if the
    ** thread is interrupted.
    */
    Semaphore.acquireUninterruptibly();
").

%---------------------------------------------------------------------------%

impure_try_wait(Sem, Res) :-
    impure impure_try_wait_2(Sem, Res0),
    Res = ( if Res0 = 0 then yes else no ).

:- impure pred impure_try_wait_2(semaphore::in, int::out) is det.

:- pragma foreign_proc("C",
    impure_try_wait_2(Semaphore::in, Res::out),
    [will_not_call_mercury, thread_safe],
"
    ML_Semaphore    *sem;

    sem = (ML_Semaphore *) Semaphore;

    MR_LOCK(&(sem->lock), ""semaphore.try_wait"");
    if (sem->count > 0) {
        sem->count--;
        MR_UNLOCK(&(sem->lock), ""semaphore.try_wait"");
        Res = 0;
    } else {
        MR_UNLOCK(&(sem->lock), ""semaphore.try_wait"");
        Res = 1;
    }
").

:- pragma foreign_proc("C#",
    impure_try_wait_2(Semaphore::in, Res::out),
    [will_not_call_mercury, thread_safe],
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

:- pragma foreign_proc("Java",
    impure_try_wait_2(Semaphore::in, Res::out),
    [will_not_call_mercury, thread_safe],
"
    Res = Semaphore.tryAcquire() ? 0 : 1;
").

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C",
"
/*
INIT mercury_sys_init_semaphore_modules
*/

#ifndef MR_HIGHLEVEL_CODE
    MR_define_extern_entry(mercury__thread__semaphore__nop);
#endif
").

:- pragma foreign_code("C",
"
#ifndef MR_HIGHLEVEL_CODE

    MR_BEGIN_MODULE(hand_written_semaphores_module)
        MR_init_entry_ai(mercury__thread__semaphore__nop);
    MR_BEGIN_CODE

    MR_define_entry(mercury__thread__semaphore__nop);
    {
        MR_proceed();
    }
    MR_END_MODULE

#endif

    /* forward decls to suppress gcc warnings */
    void mercury_sys_init_semaphore_modules_init(void);
    void mercury_sys_init_semaphore_modules_init_type_tables(void);
    #ifdef  MR_DEEP_PROFILING
    void mercury_sys_init_semaphore_modules_write_out_proc_statics(
        FILE *deep_fp, FILE *procrep_fp);
    #endif

    void mercury_sys_init_semaphore_modules_init(void)
    {
    #ifndef MR_HIGHLEVEL_CODE
        hand_written_semaphores_module();
    #endif
    }

    void mercury_sys_init_semaphore_modules_init_type_tables(void)
    {
        /* no types to register */
    }

    #ifdef  MR_DEEP_PROFILING
    void mercury_sys_init_semaphore_modules_write_out_proc_statics(
        FILE *deep_fp, FILE *procrep_fp)
    {
        /* no proc_statics to write out */
    }
    #endif
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
