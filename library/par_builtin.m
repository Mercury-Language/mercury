%---------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sw=4 sts=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% File: par_builtin.m.
% Main authors: wangp.
% Stability: low.

% This file is automatically imported, as if via `use_module', into every
% module in lowlevel parallel grades.  It is intended for the builtin procedures
% that the compiler generates implicit calls to when implementing parallel
% conjunctions.
%
% This module is a private part of the Mercury implementation; user modules
% should never explicitly import this module. The interface for this module
% does not get included in the Mercury library reference manual.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module par_builtin.

%-----------------------------------------------------------------------------%

:- interface.

:- type future(T).

    % Allocate a new future object.  A future acts as a intermediary for a
    % shared variable between parallel conjuncts, when one conjunct produces
    % the value for other conjuncts.
    %
:- pred new_future(future(T)::uo) is det.

    % wait(Future, Value)
    % Wait until Future is signalled, blocking if necessary.  Then the value
    % bound to the variable associated with the future is bound to Value.
    %
:- pred wait(future(T)::in, T::out) is det.

    % get(Future, Value)
    % Like wait but assumes the future has been signalled already.
    %
:- pred get(future(T)::in, T::out) is det.

    % Notify that the variable associated with the given future has been bound
    % to a value.  Threads waiting on the future will be woken.  Future waits
    % on the future will succeed immediately.  A future can only be signalled
    % once.
    %
:- impure pred signal(future(T)::in, T::in) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C",
"
    #include ""mercury_context.h""
    #include ""mercury_thread.h""

    typedef struct MR_Future MR_Future;

#ifdef MR_THREAD_SAFE
    struct MR_Future {
        MercuryLock lock;
            /* lock preventing concurrent accesses */
        int signalled;
            /* whether this future has been signalled yet */
        MR_Context *suspended;
            /* linked list of all the contexts blocked on this future */
        MR_Word value;
    };
#else /* !MR_THREAD_SAFE */
    struct MR_Future {
    };
#endif /* !MR_THREAD_SAFE */
").

:- pragma foreign_type("C", future(T), "MR_Future *",
    [can_pass_as_mercury_type]).

    % Placeholder only.
:- pragma foreign_type(il, future(T), "class [mscorlib]System.Object").

:- pragma foreign_proc("C",
    new_future(Future::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)

    MR_Word fut_addr;

    MR_incr_hp(fut_addr, MR_round_up(sizeof(MR_Future), sizeof(MR_Word)));
    Future = (MR_Future *) fut_addr;

    pthread_mutex_init(&(Future->lock), MR_MUTEX_ATTR);

    /*
    ** The mutex needs to be destroyed when the future is garbage collected.
    ** For efficiency we might want to ignore this altogether, e.g. on Linux
    ** pthread_mutex_destroy() only checks that the mutex is unlocked.
    */
  #ifdef MR_CONSERVATIVE_GC
    GC_REGISTER_FINALIZER(Future, MR_finalize_future, NULL, NULL, NULL);
  #endif

    Future->signalled = 0;
    Future->suspended = NULL;
    Future->value = 0;

#else

    MR_fatal_error(""internal error: par_builtin should only be used by ""
        ""lowlevel parallel grades"");

#endif
").

:- pragma foreign_decl("C", "
#ifdef MR_CONSERVATIVE_GC
    void MR_finalize_future(void *obj, void *cd);
#endif
").

:- pragma foreign_code("C", "
#ifdef MR_CONSERVATIVE_GC
    void
    MR_finalize_future(void *obj, void *cd)
    {
        MR_Future *fut = (MR_Future *) obj;

      #ifdef MR_THREAD_SAFE
        pthread_mutex_destroy(&(fut->lock));
      #endif
    }
#endif
").

:- pragma foreign_proc("C",
    wait(Future::in, Value::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)

    MR_LOCK(&(Future->lock), ""future.wait"");

    if (Future->signalled) {
        Value = Future->value;
        MR_UNLOCK(&(Future->lock), ""future.wait"");
    } else {
        MR_Context *ctxt;
        MercuryThreadList *new_element;

        /*
        ** The address of the future can be lost when we resume so save it on
        ** top of the stack.
        */
        MR_incr_sp(1);
        MR_sv(1) = (MR_Word) Future;

        ctxt = MR_ENGINE(MR_eng_this_context);

        /*
        ** Mark the current context as being owned by this thread to prevent it
        ** from being resumed by another thread. Specifically we don't want the
        ** 'main' context to be resumed by any thread other than the primordial
        ** thread, because after the primordial thread finishes executing the
        ** main program it has to clean up the Mercury runtime.
        **
        ** XXX this solution seems too heavy for the problem at hand
        */
        MR_ENGINE(MR_eng_c_depth)++;

        new_element = MR_GC_NEW(MercuryThreadList);
        new_element->thread = ctxt->MR_ctxt_owner_thread;
        new_element->next = MR_ENGINE(MR_eng_saved_owners);
        MR_ENGINE(MR_eng_saved_owners) = new_element;

        ctxt->MR_ctxt_owner_thread = MR_ENGINE(MR_eng_owner_thread);

        /*
        ** Save this context and put it on the list of suspended contexts for
        ** this future.
        */
        MR_save_context(ctxt);
        ctxt->MR_ctxt_resume = MR_ENTRY(mercury__par_builtin__wait_resume);
        ctxt->MR_ctxt_next = Future->suspended;
        Future->suspended = ctxt;

        MR_UNLOCK(&(Future->lock), ""future.wait"");
        MR_runnext();

        assert(0);
    }

#else

    MR_fatal_error(""internal error: par_builtin.wait"");
    Value = -1;

#endif
").

    % `wait_resume' is the piece of code we jump to when a thread suspended
    % on a future resumes after the future is signalled.
    % 
:- pragma foreign_decl("C",
"
/*
INIT mercury_par_builtin_wait_resume
*/

#if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)
    MR_define_extern_entry(mercury__par_builtin__wait_resume);
#endif
").

:- pragma foreign_code("C",
"
#if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)

    MR_BEGIN_MODULE(wait_resume_module)
        MR_init_entry_ai(mercury__par_builtin__wait_resume);
    MR_BEGIN_CODE
    MR_define_entry(mercury__par_builtin__wait_resume);
    {
        MR_Future *Future;

        /* Restore the address of the future after resuming. */
        Future = (MR_Future *) MR_sv(1);
        MR_decr_sp(1);

        assert(Future->signalled == 1);

        /* Restore the owning thread in the current context. */
        assert(MR_ENGINE(MR_eng_this_context)->MR_ctxt_owner_thread
            == MR_ENGINE(MR_eng_owner_thread));
        MR_ENGINE(MR_eng_c_depth)--;
        {
            MercuryThreadList *tmp;
            MercuryThread val;

            tmp = MR_ENGINE(MR_eng_saved_owners);
            if (tmp != NULL)
            {
                val = tmp->thread;
                MR_ENGINE(MR_eng_saved_owners) = tmp->next;
                MR_GC_free(tmp);
            } else {
                val = 0;
            }
            MR_ENGINE(MR_eng_this_context)->MR_ctxt_owner_thread = val;
        }

        /* Return to the caller of par_builtin.wait. */
        MR_r1 = Future->value;
        MR_proceed();
    }
    MR_END_MODULE

#endif

    /* forward decls to suppress gcc warnings */
    void mercury_par_builtin_wait_resume_init(void);
    void mercury_par_builtin_wait_resume_init_type_tables(void);
    #ifdef  MR_DEEP_PROFILING
    void mercury_par_builtin_wait_resume_write_out_proc_statics(FILE *fp);
    #endif

    void mercury_par_builtin_wait_resume_init(void)
    {
    #if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)
        wait_resume_module();
    #endif
    }

    void mercury_par_builtin_wait_resume_init_type_tables(void)
    {
        /* no types to register */
    }

    #ifdef  MR_DEEP_PROFILING
    void mercury_par_builtin_wait_resume_write_out_proc_statics(FILE *fp)
    {
        /* no proc_statics to write out */
    }
    #endif
").

:- pragma foreign_proc("C",
    get(Future::in, Value::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)

    assert(Future->signalled == 1);
    Value = Future->value;

#else

    MR_fatal_error(""internal error: par_builtin.get"");
    Value = -1;

#endif
").

:- pragma foreign_proc("C",
    signal(Future::in, Value::in),
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"
#if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)

    MR_Context *ctxt;

    MR_LOCK(&(Future->lock), ""future.signal"");

    /*
    ** If the same future is passed twice to a procedure then it
    ** could be signalled twice, but the value must be the same.
    */
    if (Future->signalled != 0) {
        assert(Future->signalled == 1);
        assert(Future->value == Value);
    } else {
        Future->signalled++;
        Future->value = Value;
    }

    /* Schedule all the contexts which are blocking on this future. */
    ctxt = Future->suspended;
    while (ctxt != NULL) {
        MR_schedule(ctxt);
        ctxt = ctxt->MR_ctxt_next;
    }
    Future->suspended = NULL;

    MR_UNLOCK(&(Future->lock), ""future.signal"");

    assert(Future->signalled == 1);

#else

    MR_fatal_error(""internal error: par_builtin.signal"");

#endif
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
