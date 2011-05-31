%---------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sw=4 sts=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: par_builtin.m.
% Main authors: wangp.
% Stability: low.
%
% This file is automatically imported, as if via `use_module', into every
% module in lowlevel parallel grades. It is intended to hold the builtin
% procedures that the compiler generates implicit calls to when implementing
% parallel conjunctions.
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

    % Allocate a new future object. A future acts as an intermediary for a
    % shared variable between parallel conjuncts, when one conjunct produces
    % the value for other conjuncts.
    %
    % The first argument is an integer that refers to a string (via a table) of
    % the future's name. It is used in threadscope grades.
    %
:- pred new_future(int::in, future(T)::uo) is det.

    % wait_future(Future, Var):
    %
    % Wait until Future is signalled, blocking if necessary. Then set Var
    % to the value bound to the variable associated with the future.
    %
    % wait_future/2 doesn't actually have a side effect.  However once it has
    % returned get_future/2 is guaranteed to be safe.  Therefore it must be
    % impure to prevent it from being optimized away, especially in (valid)
    % cases where its output occurs only once in a procedure.
    %
:- impure pred wait_future(future(T)::in, T::out) is det.

    % get_future(Future, Var):
    %
    % Like wait_future but assumes that the future has been signalled already.
    %
:- pred get_future(future(T)::in, T::out) is det.

    % signal_future(Future, Value):
    %
    % Notify any waiting threads that the variable associated with Future
    % has been bound to Value. Threads waiting on Future will be woken up,
    % and any later waits on Future will succeed immediately. A future can
    % only be signalled once.
    %
    % XXX The implementation actually allows a future to be signalled
    % more than once, provided the signals specify the same value.
    %
:- impure pred signal_future(future(T)::in, T::in) is det.

%---------------------------------------------------------------------------%

% The following predicates are intended to be used as conditions to decide
% whether the conjuncts of a conjunction should be executed in parallel or
% in sequence. Success should indicate a preference for parallel execution,
% failure a preference for sequential execution.
%
% They can be used both by compiler transformations and directly in user code.
% The latter is useful for testing.

    % A hook for the compiler's granularity transformation to hang
    % an arbitrary test on. This predicate does not have a definition;
    % it is simply used as a hook by the compiler.
    %
:- impure pred evaluate_parallelism_condition is semidet.

    % num_os_threads(Num)
    %
    % Num is the number of OS threads the runtime is configured to use, which
    % the runtime records in the variable MR_num_threads. This is the value
    % given by the user as the argument of the -P option in the MERCURY_OPTIONS
    % environment variable.
    %
:- pred num_os_threads(int::out) is det.

    % Close the file that was used to log the parallel condition decisions.
    %
    % The parallel condition stats file is opened the first time it is
    % required. If it is not open this predicate will do nothing. The recording
    % of statistics and the management of the parallel condition stats file
    % is enabled only if both of the following C compiler macros are set:
    % MR_LL_PARALLEL_CONJ and MR_DEBUG_RUNTIME_GRANULARITY_CONTROL.
    % This predicate will throw an exception unless both of these are set.
    %
:- pred par_cond_close_stats_file(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("C", future(T), "MR_Future *",
    [can_pass_as_mercury_type]).

    % Placeholders only.
:- pragma foreign_type(il, future(T), "class [mscorlib]System.Object").
:- pragma foreign_type("Erlang", future(T), "").
:- pragma foreign_type("C#", future(T), "object").
:- pragma foreign_type("Java", future(T), "java.lang.Object").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    new_future(Name::in, Future::uo),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        may_not_duplicate],
"
    #ifdef MR_THREADSCOPE
        MR_par_builtin_new_future(Future, Name);
    #else
        MR_par_builtin_new_future(Future);
    #endif
").

:- pragma foreign_proc("C",
    wait_future(Future::in, Var::out),
    [will_not_call_mercury, thread_safe, will_not_modify_trail,
        may_not_duplicate],
"
    MR_par_builtin_wait_future(Future, Var);
").

:- pragma foreign_proc("C",
    get_future(Future::in, Var::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail,
        may_not_duplicate],
"
    MR_par_builtin_get_future(Future, Var);
").

:- pragma foreign_proc("C",
    signal_future(Future::in, Value::in),
    [will_not_call_mercury, thread_safe, will_not_modify_trail,
        may_not_duplicate],
"
    MR_par_builtin_signal_future(Future, Value);
").

:- pragma foreign_proc("C",
    evaluate_parallelism_condition,
    [will_not_call_mercury, thread_safe],
"
    /* All uses of this predicate should override the body. */
    MR_fatal_error(""evaluate_parallelism_condition called"");
").

%-----------------------------------------------------------------------------%

    % `wait_resume' is the piece of code we jump to when a thread suspended
    % on a future resumes after the future is signalled.
    %
:- pragma foreign_decl("C",
"
/*
INIT mercury_sys_init_par_builtin_modules
*/

#if !defined(MR_HIGHLEVEL_CODE) && defined(MR_THREAD_SAFE)
    MR_define_extern_entry(mercury__par_builtin__wait_resume);
#endif
").

:- pragma foreign_code("C",
"
#if !defined(MR_HIGHLEVEL_CODE) && defined(MR_THREAD_SAFE)

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

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    num_os_threads(NThreads::out),
    [will_not_call_mercury, will_not_throw_exception, thread_safe,
        promise_pure],
"
    /*
    ** MR_num_threads is available in all grades. Although it won't make sense
    ** for non-parallel grades, it will still reflect the value configured by
    ** the user.
    */
    NThreads = MR_num_threads
").

:- pragma foreign_proc("C",
    par_cond_close_stats_file(IO0::di, IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
#if defined(MR_LL_PARALLEL_CONJ) && \
        defined(MR_DEBUG_RUNTIME_GRANULARITY_CONTROL)
    MR_write_out_conditional_parallelism_log();
#else
    MR_fatal_error(""par_cond_close_stats_file is unavailable in this grade"");
#endif
    IO = IO0;
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
