%---------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sw=4 sts=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2009 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
% 
% File: par_builtin.m.
% Main authors: wangp.
% Stability: low.
% 
% This file is automatically imported, as if via `use_module', into every
% module in lowlevel parallel grades.  It is intended for the builtin procedures
% that the compiler generates implicit calls to when implementing parallel
% conjunctions.
%
% This module is a private part of the Mercury implementation; user modules
% should never explicitly import this module. The interface for this module
% does not get included in the Mercury library reference manual.
% 
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module par_builtin.
:- interface.

:- import_module io.

:- type future(T).

    % Allocate a new future object.  A future acts as an intermediary for a
    % shared variable between parallel conjuncts, when one conjunct produces
    % the value for other conjuncts.
    %
:- pred new_future(future(T)::uo) is det.

    % wait(Future, Value):
    %
    % Wait until Future is signalled, blocking if necessary.  Then the value
    % bound to the variable associated with the future is bound to Value.
    %
:- pred wait(future(T)::in, T::out) is det.

    % get(Future, Value):
    %
    % Like wait but assumes the future has been signalled already.
    %
:- pred get(future(T)::in, T::out) is det.

    % Notify that the variable associated with the given future has been bound
    % to a value.  Threads waiting on the future will be woken.  Future waits
    % on the future will succeed immediately.  A future can only be signalled
    % once.
    %
:- impure pred signal(future(T)::in, T::in) is det.

% The following predicates are intended to be used as conditions to decide if
% something should be done in parallel or sequence. Success should indicate
% a preference for parallel execution, failure a preference for sequential
% execution.
%
% They can be used both by compiler transformations and directly in user code.
% The latter is is useful for testing.

    % A hook for the compiler's granularity transformation to hang
    % an arbitrary test on.  This predicate does not have a definition, it is
    % simply used as a hook by the compiler.
    %
:- impure pred evaluate_parallelism_condition is semidet.

    % par_cond_outstanding_jobs_vs_num_cpus(NumCPUs)
    %
    % True iff NumCPUs > executable contexts + global sparks.
    %
    % Consider passing MR_num_threads as the argument.
    %
:- impure pred par_cond_outstanding_jobs_vs_num_cpus(int::in) is semidet.

    % Close the file that was used to log the parallel condition decisions.
    %
:- pred par_cond_close_stats_file(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C",
"
    #include ""mercury_context.h""
    #include ""mercury_thread.h""

    typedef struct MR_Future MR_Future;

#ifdef MR_THREAD_SAFE
    struct MR_Future {
        /* lock preventing concurrent accesses */
        MercuryLock     MR_fut_lock;
        /* whether this future has been signalled yet */
        int             MR_fut_signalled;

        /* linked list of all the contexts blocked on this future */
        MR_Context      *MR_fut_suspended;
        MR_Word         MR_fut_value;
    };
#else /* !MR_THREAD_SAFE */
    struct MR_Future {
        char dummy; /* ANSI C doesn't allow empty structs */
    };
#endif /* !MR_THREAD_SAFE */
").

:- pragma foreign_type("C", future(T), "MR_Future *",
    [can_pass_as_mercury_type]).

    % Placeholder only.
:- pragma foreign_type(il, future(T), "class [mscorlib]System.Object").
:- pragma foreign_type("Erlang", future(T), "").
:- pragma foreign_type("Java", future(T), "java.lang.Object").

:- pragma foreign_proc("C",
    new_future(Future::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)

    MR_Word fut_addr;

    MR_incr_hp(fut_addr, MR_round_up(sizeof(MR_Future), sizeof(MR_Word)));
    Future = (MR_Future *) fut_addr;

    pthread_mutex_init(&(Future->MR_fut_lock), MR_MUTEX_ATTR);

    /*
    ** The mutex needs to be destroyed when the future is garbage collected.
    ** For efficiency we might want to ignore this altogether, e.g. on Linux
    ** pthread_mutex_destroy() only checks that the mutex is unlocked.
    **
    ** We initialize the value field only to prevent its previous value,
    ** which may point to an allocated block, keeping that block alive.
    ** Semantically, the value field is undefined at this point in time.
    */
  #ifdef MR_CONSERVATIVE_GC
    GC_REGISTER_FINALIZER(Future, MR_finalize_future, NULL, NULL, NULL);
    Future->MR_fut_value = 0;
  #endif

    Future->MR_fut_signalled = MR_FALSE;
    Future->MR_fut_suspended = NULL;

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
        pthread_mutex_destroy(&(fut->MR_fut_lock));
      #endif
    }
#endif
").

:- pragma foreign_proc("C",
    wait(Future::in, Value::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)

    /*
    ** It would be nice if we could rely on an invariant such as
    ** `if MR_fut_signalled is true, then reading MR_fut_value is ok'
    ** even *without* wrapping up those two field accesses in the mutex,
    ** taking the mutex only when MR_fut_signalled is false. (We would
    ** then have to repeat the test of MR_fut_signalled, of course.)
    ** Unfortunately, memory systems today cannot be relied on to provide
    ** the required level of consistency.
    */

    MR_LOCK(&(Future->MR_fut_lock), ""future.wait"");

    if (Future->MR_fut_signalled) {
        Value = Future->MR_fut_value;
        MR_UNLOCK(&(Future->MR_fut_lock), ""future.wait"");
    } else {
        MR_Context *ctxt;

        /*
        ** Put the address of the future at a fixed place known to
        ** mercury__par_builtin__wait_resume, to wit, the top of the stack.
        */
        MR_incr_sp(1);
        MR_sv(1) = (MR_Word) Future;

        /*
        ** Save this context and put it on the list of suspended contexts for
        ** this future.
        */
        ctxt = MR_ENGINE(MR_eng_this_context);
        MR_save_context(ctxt);

        ctxt->MR_ctxt_resume = MR_ENTRY(mercury__par_builtin__wait_resume);
        ctxt->MR_ctxt_next = Future->MR_fut_suspended;
        Future->MR_fut_suspended = ctxt;

        MR_UNLOCK(&(Future->MR_fut_lock), ""future.wait"");

        MR_ENGINE(MR_eng_this_context) = NULL;
        MR_runnext();
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
INIT mercury_sys_init_par_builtin_modules
*/

#if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)
    MR_define_extern_entry(mercury__par_builtin__wait_resume);
#endif
").

:- pragma foreign_code("C",
"
#if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)

    MR_BEGIN_MODULE(hand_written_par_builtin_module)
        MR_init_entry_ai(mercury__par_builtin__wait_resume);
    MR_BEGIN_CODE

    MR_define_entry(mercury__par_builtin__wait_resume);
    {
        MR_Future *Future;

        /* Restore the address of the future after resuming. */
        Future = (MR_Future *) MR_sv(1);
        MR_decr_sp(1);

        assert(Future->MR_fut_signalled);

        /* Return to the caller of par_builtin.wait. */
        MR_r1 = Future->MR_fut_value;
        MR_proceed();
    }
    MR_END_MODULE

#endif

    /* forward decls to suppress gcc warnings */
    void mercury_sys_init_par_builtin_modules_init(void);
    void mercury_sys_init_par_builtin_modules_init_type_tables(void);
    #ifdef  MR_DEEP_PROFILING
    void mercury_sys_init_par_builtin_modules_write_out_proc_statics(
        FILE *deep_fp, FILE *procrep_fp);
    #endif

    void mercury_sys_init_par_builtin_modules_init(void)
    {
    #if (!defined MR_HIGHLEVEL_CODE) && (defined MR_THREAD_SAFE)
        hand_written_par_builtin_module();
    #endif
    }

    void mercury_sys_init_par_builtin_modules_init_type_tables(void)
    {
        /* no types to register */
    }

    #ifdef  MR_DEEP_PROFILING
    void mercury_sys_init_par_builtin_modules_write_out_proc_statics(
        FILE *deep_fp, FILE *procrep_fp)
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

    assert(Future->MR_fut_signalled);
    Value = Future->MR_fut_value;

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
    MR_Context *next;

    MR_LOCK(&(Future->MR_fut_lock), ""future.signal"");

    /*
    ** If the same future is passed twice to a procedure then it
    ** could be signalled twice, but the value must be the same.
    */
    if (Future->MR_fut_signalled) {
        assert(Future->MR_fut_value == Value);
    } else {
        Future->MR_fut_signalled = MR_TRUE;
        Future->MR_fut_value = Value;
    }

    /* Schedule all the contexts which are blocking on this future. */
    ctxt = Future->MR_fut_suspended;
    while (ctxt != NULL) {
        next = ctxt->MR_ctxt_next;
        MR_schedule_context(ctxt);  /* clobbers MR_ctxt_next */
        ctxt = next;
    }
    Future->MR_fut_suspended = NULL;

    MR_UNLOCK(&(Future->MR_fut_lock), ""future.signal"");

#else

    MR_fatal_error(""internal error: par_builtin.signal"");

#endif
").

:- pragma foreign_proc("C",
    evaluate_parallelism_condition,
    [will_not_call_mercury, thread_safe],
"
    /* All uses of this predicate should override the body. */
    MR_fatal_error(""evaluate_parallelism_condition called"");
").

:- pragma foreign_proc("C",
    par_cond_outstanding_jobs_vs_num_cpus(NumCPUs::in),
    [will_not_call_mercury, thread_safe, may_not_duplicate],
"
#ifdef MR_LL_PARALLEL_CONJ
    SUCCESS_INDICATOR = MR_choose_parallel_over_sequential_cond(NumCPUs);
  #ifdef MR_DEBUG_RUNTIME_GRANULARITY_CONTROL
    MR_record_conditional_parallelism_descision(SUCCESS_INDICATOR);
  #endif
#else
    MR_fatal_error(
      ""par_cond_outstanding_jobs_vs_num_cpus is unavailable in this grade"");
#endif
").

:- pragma foreign_proc("C",
    par_cond_close_stats_file(IO0::di, IO::uo),
    [will_not_call_mercury, thread_safe, may_not_duplicate, promise_pure],
"
#ifdef MR_LL_PARALLEL_CONJ
  #ifdef MR_DEBUG_RUNTIME_GRANULARITY_CONTROL
    MR_write_out_conditional_parallelism_log();
  #else
    MR_fatal_error(""par_cond_close_stats_file is unavailable in this build"");
  #endif
#else
    MR_fatal_error(""par_cond_close_stats_file is unavailable in this grade"");
#endif
    IO = IO0; 
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
