%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2000-2001, 2003-2004, 2006-2008, 2010-2011 The University
% of Melbourne.
% Copyright (C) 2014-2026 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: thread.m.
% Authors: conway, wangp.
% Stability: medium.
%
% This module defines the Mercury concurrency interface.
%
% The term `concurrency' refers to threads, not necessarily to parallel
% execution of those threads. (The latter is also possible if you are using
% one of the .par grades or the Java or C# backends.)
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module thread.
:- interface.

:- import_module io.
:- import_module maybe.

:- include_module barrier.
:- include_module channel.
:- include_module closeable_channel.
:- include_module future.
:- include_module mvar.
:- include_module semaphore.

%---------------------------------------------------------------------------%

    % Abstract type representing a detached thread.
    %
:- type thread.

    % Abstract type representing a joinable thread.
    %
:- type joinable_thread(T).

    % can_spawn succeeds if spawn/4 is supported in the current grade.
    %
:- pred can_spawn is semidet.

    % can_spawn_native succeeds if spawn_native/4 is supported in the current
    % grade.
    %
:- pred can_spawn_native is semidet.

    % spawn(Closure, IO0, IO) is true if-and-only-if IO0 denotes a list of I/O
    % transactions that is an interleaving of those performed by Closure
    % and those contained in IO - the list of transactions performed by
    % the continuation of spawn/3.
    %
    % Operationally, spawn/3 is like spawn/4 except that Closure does not
    % accept a thread handle argument, and an exception is thrown if the
    % thread cannot be created.
    %
:- pred spawn(pred(io, io)::in(pred(di, uo) is cc_multi),
    io::di, io::uo) is cc_multi.

    % spawn(Closure, Res, IO0, IO) creates a new thread and performs Closure in
    % that thread. On success it returns ok(Thread) where Thread is a handle to
    % the new thread. Otherwise it returns an error.
    %
:- pred spawn(pred(thread, io, io)::in(pred(in, di, uo) is cc_multi),
    maybe_error(thread)::out, io::di, io::uo) is cc_multi.

    % A type representing options that affect thread creation.
    %
:- type thread_options.

    % Create a new thread options object with options set to their default
    % values. The options are:
    %
    % - min_stack_size: the minimum stack size in bytes (default: 0).
    %   The special value 0 means to use the default stack size as chosen by
    %   the underlying environment.
    %
:- func init_thread_options = thread_options.

    % Set the minimum stack size (in bytes) for a new thread created with these
    % thread options. This only affects C grades that use POSIX threads.
    % The Java and C# backends do not yet respect the minimum stack size
    % option.
    %
:- pred set_min_stack_size(uint::in, thread_options::in, thread_options::out)
    is det.

    % spawn_native(Closure, Res, !IO):
    % Same as spawn_native(Closure, init_thread_options, Res, !IO).
    %
:- pred spawn_native(pred(thread, io, io)::in(pred(in, di, uo) is cc_multi),
    maybe_error(thread)::out, io::di, io::uo) is cc_multi.

    % spawn_native(Closure, Options, Res, IO0, IO):
    % Like spawn/4, but Closure will be performed in a separate "native thread"
    % of the environment the program is running in (POSIX thread, Windows
    % thread, Java thread, etc.).
    %
    % spawn_native exposes a low-level implementation detail, so it is more
    % likely to change with the implementation.
    %
    % Rationale: on the low-level C backend Mercury threads are multiplexed
    % onto a limited number of OS threads. A call to a blocking procedure
    % prevents that OS thread from making progress on another Mercury thread.
    % Also, some foreign code depends on OS thread-local state so needs to be
    % consistently executed on a dedicated OS thread to be usable.
    %
:- pred spawn_native(pred(thread, io, io)::in(pred(in, di, uo) is cc_multi),
    thread_options::in, maybe_error(thread)::out, io::di, io::uo) is cc_multi.

    % spawn_native_joinable(Closure, Options, Res, IO0, IO);
    %
    % Create a joinable native thread (like spawn_native), then perform Closure
    % in that thread. Another thread can call join_thread/4 to wait for the
    % thread to terminate, and fetch the output returned by Closure.
    % The thread will continue to take up system resources until it terminates
    % and has been joined by a call to join_thread/4.
    %
:- pred spawn_native_joinable(
    pred(joinable_thread(T), T, io, io)::in(pred(in, out, di, uo) is cc_multi),
    thread_options::in, maybe_error(joinable_thread(T))::out, io::di, io::uo)
    is cc_multi.

    % join_thread(Thread, Res, !IO):
    %
    % Wait for the specified thread to terminate. If the thread has already
    % terminated, join_thread/4 will return immediately. On success, Res will
    % be ok(Output) where Output is the value returned by the closure
    % performed on that thread.
    %
    % A thread must only be joined once. If multiple threads simultaneously
    % try to join with the same thread, the results are undefined.
    %
:- pred join_thread(joinable_thread(T)::in, maybe_error(T)::out,
    io::di, io::uo) is cc_multi.

    % yield(IO0, IO) is logically equivalent to (IO = IO0), but
    % operationally, yields the Mercury engine to some other thread
    % if one exists.
    %
    % NOTE: this is not yet implemented in the hl*.par.gc grades; currently
    % it is a no-op in those grades.
    %
:- pred yield(io::di, io::uo) is det.

    % num_processors(Num, !IO)
    %
    % Retrieve the number of processors available to this process for
    % parallel execution, if known.
    %
    % Note that the number of available processors can be different from the
    % actual number of processors/cores:
    %
    %  + It includes hardware threads.
    %  + The Mercury grade may restrict the process to one processor.
    %  + The OS may be configured to restrict the number of processors
    %    available (e.g. cpuset(7) on Linux).
    %
:- pred num_processors(maybe(int)::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module mutvar.
:- import_module require.

:- pragma foreign_decl("C", "
#ifndef MR_HIGHLEVEL_CODE
  #if (!defined(MR_EXEC_TRACE) && !defined(MR_DEEP_PROFILING)) \
        || !defined(MR_USE_GCC_NONLOCAL_GOTOS)
    // In calling thread.yield, semaphore.wait or semaphore.signal,
    // the calling context may need to suspend and yield to another context.
    // This is implemented by setting the resume address of the context to
    // an auxiliary function outside of the foreign_proc. This breaks when
    // execution tracing or deep profiling are enabled as code inserted at the
    // end of the foreign_proc won't be executed. In those cases we rely on
    // the gcc extension that allows us to take the address of labels within
    // the foreign_proc, so the context will resume back inside the
    // foreign_proc.
    //
    // XXX Implement those procedures as :- pragma external_preds so that the
    // transforms won't be applied.
    #define ML_THREAD_AVOID_LABEL_ADDRS
  #endif
#endif

#ifdef MR_THREAD_SAFE
    typedef pthread_t   ML_ThreadHandle;
#else
    typedef MR_Word     ML_ThreadHandle;
#endif
").

:- pragma foreign_decl("Java", "
import jmercury.runtime.JavaInternal;
import jmercury.runtime.MercuryThread;
import jmercury.runtime.Task;
").

:- type thread_options
    --->    thread_options(
                min_stack_size  :: uint
            ).

:- type thread
    --->    detached_thread(thread_desc).

:- type joinable_thread(T)
    --->    joinable_thread(
                jt_handle   :: thread_handle,
                jt_mutvar   :: mutvar(T)
            ).

    % A descriptor for a (detached) Mercury thread.
    % thread_desc values are not publicly exported, but they may help with
    % debugging by printing and/or comparing of 'thread' values. There is
    % no guarantee that a thread descriptor remains unique after a thread
    % terminates, as the memory address used to derive the descriptor
    % may be reused.
    %
:- type thread_desc == string.

    % A thread handle from the underlying thread API.
    %
:- type thread_handle.
:- pragma foreign_type("C", thread_handle, "ML_ThreadHandle").
:- pragma foreign_type("C#", thread_handle, "System.Threading.Thread").
:- pragma foreign_type("Java", thread_handle,
    "jmercury.runtime.MercuryThread").

%---------------------------------------------------------------------------%

can_spawn :-
    ( can_spawn_context
    ; can_spawn_native
    ).

:- pred can_spawn_context is semidet.

:- pragma foreign_proc("C",
    can_spawn_context,
    [will_not_call_mercury, promise_pure, thread_safe, may_not_duplicate],
"
#if !defined(MR_HIGHLEVEL_CODE) && defined(MR_THREAD_SAFE)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").
:- pragma foreign_proc("Java",
    can_spawn_context,
    [will_not_call_mercury, promise_pure],
"
    SUCCESS_INDICATOR = true;
").

can_spawn_context :-
    semidet_fail.

:- pragma foreign_proc("C",
    can_spawn_native,
    [will_not_call_mercury, promise_pure, thread_safe],
"
#if defined(MR_THREAD_SAFE)
    SUCCESS_INDICATOR = MR_TRUE;
#else
    SUCCESS_INDICATOR = MR_FALSE;
#endif
").
:- pragma foreign_proc("C#",
    can_spawn_native,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").
:- pragma foreign_proc("Java",
    can_spawn_native,
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = true;
").

%---------------------------------------------------------------------------%

spawn(Goal0, !IO) :-
    Goal = (pred(_Thread::in, IO0::di, IO::uo) is cc_multi :- Goal0(IO0, IO)),
    spawn(Goal, Res, !IO),
    (
        Res = ok(_)
    ;
        Res = error(Error),
        unexpected($pred, Error)
    ).

spawn(Goal, Res, !IO) :-
    ( if can_spawn_context then
        spawn_context(Goal, Res, !IO)
    else
        spawn_native(Goal, Res, !IO)
    ).

%---------------------------------------------------------------------------%

:- pred spawn_context(pred(thread, io, io)::in(pred(in, di, uo) is cc_multi),
    maybe_error(thread)::out, io::di, io::uo) is cc_multi.

spawn_context(Goal, Res, !IO) :-
    spawn_context_2(Goal, Success, ThreadDesc, !IO),
    (
        Success = yes,
        Res = ok(detached_thread(ThreadDesc))
    ;
        Success = no,
        Res = error("Unable to spawn threads in this grade.")
    ).

:- pred spawn_context_2(pred(thread, io, io)::in(pred(in, di, uo) is cc_multi),
    bool::out, thread_desc::out, io::di, io::uo) is cc_multi.

spawn_context_2(_, Res, "", !IO) :-
    ( Res = no
    ; Res = no
    ).

:- pragma foreign_proc("C",
    spawn_context_2(Goal::in(pred(in, di, uo) is cc_multi), Success::out,
        ThreadDesc::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#if !defined(MR_HIGHLEVEL_CODE)
{
    MR_Context          *ctxt;
    MR_ThreadLocalMuts  *tlm;

    ML_incr_thread_barrier_count();

    ctxt = MR_create_context(""spawn"", MR_CONTEXT_SIZE_REGULAR, NULL);
    ctxt->MR_ctxt_resume = MR_ENTRY(mercury__thread__spawn_begin_thread);

    tlm = MR_clone_thread_local_mutables(MR_THREAD_LOCAL_MUTABLES);
    ctxt->MR_ctxt_thread_local_mutables = tlm;

    // Derive a thread descriptor from the address of the thread-local mutable
    // vector for the Mercury thread. It should actually be more unique than a
    // context address as contexts are kept around and reused.
    ThreadDesc = MR_make_string(MR_ALLOC_ID, ""%p"", tlm);

    // Store Goal and ThreadDesc on the top of the new context's stack.
    ctxt->MR_ctxt_sp += 2;
    ctxt->MR_ctxt_sp[0] = Goal;                     // MR_stackvar(1)
    ctxt->MR_ctxt_sp[-1] = (MR_Word) ThreadDesc;    // MR_stackvar(2)

    MR_schedule_context(ctxt);

    Success = MR_TRUE;
}
#else // MR_HIGHLEVEL_CODE
{
    Success = MR_FALSE;
    ThreadDesc = MR_make_string_const("""");
}
#endif // MR_HIGHLEVEL_CODE
").

:- pragma foreign_proc("Java",
    spawn_context_2(Goal::in(pred(in, di, uo) is cc_multi), Success::out,
        ThreadDesc::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    RunGoalDetached rg = new RunGoalDetached((Object[]) Goal);
    Task task = new Task(rg);
    ThreadDesc = String.valueOf(task.getId());
    rg.setThreadDesc(ThreadDesc);
    JavaInternal.getThreadPool().submit(task);
    Success = bool.YES;
").

%---------------------------------------------------------------------------%

init_thread_options = thread_options(0u).

set_min_stack_size(MinStackSize, !Options) :-
    !Options ^ min_stack_size := MinStackSize.

%---------------------------------------------------------------------------%

spawn_native(Goal, Res, !IO) :-
    spawn_native(Goal, init_thread_options, Res, !IO).

spawn_native(Goal, Options, Res, !IO) :-
    Options = thread_options(MinStackSize),
    Dummy = 0,  % for the typeinfo
    spawn_native_2(Goal, Dummy, MinStackSize, Success, ThreadDesc, ErrorMsg,
        !IO),
    (
        Success = yes,
        Res = ok(detached_thread(ThreadDesc))
    ;
        Success = no,
        Res = error(ErrorMsg)
    ).

:- pred spawn_native_2(pred(thread, io, io)::in(pred(in, di, uo) is cc_multi),
    T::in, uint::in, bool::out, thread_desc::out, string::out,
    io::di, io::uo) is cc_multi.

:- pragma foreign_proc("C",
    spawn_native_2(Goal::in(pred(in, di, uo) is cc_multi), _T::in,
        MinStackSize::in, Success::out, ThreadDesc::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MR_THREAD_SAFE
    pthread_t   thread_handle;

    // Pass 0 for joinable_thread_mutvar to create a detached thread.
    Success = ML_create_exclusive_thread(TypeInfo_for_T, Goal,
        MinStackSize, (MR_Word) 0, &ThreadDesc, &thread_handle, &ErrorMsg,
        MR_ALLOC_ID);
#else
    Success = MR_FALSE;
    ThreadDesc = MR_make_string_const("""");
    ErrorMsg = MR_make_string_const(
        ""Cannot create native thread in this grade."");
#endif
").

:- pragma foreign_proc("C#",
    spawn_native_2(Goal::in(pred(in, di, uo) is cc_multi), _T::in,
        _MinStackSize::in, Success::out, ThreadDesc::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    try {
        object[] thread_locals = runtime.ThreadLocalMutables.clone();
        RunGoalDetached rg = new RunGoalDetached(Goal, thread_locals);
        System.Threading.Thread thread = new System.Threading.Thread(
            new System.Threading.ThreadStart(rg.run));
        string thread_desc = thread.ManagedThreadId.ToString();
        rg.setThreadDesc(thread_desc);
        thread.Start();

        Success = mr_bool.YES;
        ThreadDesc = thread_desc;
        ErrorMsg = """";
    } catch (System.SystemException e) {
        // This includes System.Threading.ThreadStartException.
        // SystemException has been seen with mono.
        Success = mr_bool.NO;
        ThreadDesc = """";
        ErrorMsg = e.Message;
    }
").

:- pragma foreign_proc("Java",
    spawn_native_2(Goal::in(pred(in, di, uo) is cc_multi), _T::in,
        _MinStackSize::in, Success::out, ThreadDesc::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    try {
        RunGoalDetached rg = new RunGoalDetached((Object[]) Goal);
        Task task = new Task(rg);
        String thread_desc = String.valueOf(task.getId());
        rg.setThreadDesc(thread_desc);
        JavaInternal.getThreadPool().createExclusiveThread(task).start();

        Success = bool.YES;
        ThreadDesc = thread_desc;
        ErrorMsg = """";
    } catch (java.lang.SecurityException e) {
        Success = bool.NO;
        ThreadDesc = """";
        ErrorMsg = e.getMessage();
    } catch (java.lang.OutOfMemoryError e) {
        Success = bool.NO;
        ThreadDesc = """";
        ErrorMsg = e.getMessage();
    }
    if (Success == bool.NO && ErrorMsg == null) {
        ErrorMsg = ""Unable to create new native thread."";
    }
").

%---------------------------------------------------------------------------%

spawn_native_joinable(Goal, Options, Res, !IO) :-
    Options = thread_options(MinStackSize),
    promise_pure (
        impure new_mutvar0(OutputMutvar),
        spawn_native_joinable_2(Goal, MinStackSize, OutputMutvar,
            Success, ThreadHandle, ErrorMsg, !IO)
    ),
    (
        Success = yes,
        Res = ok(joinable_thread(ThreadHandle, OutputMutvar))
    ;
        Success = no,
        Res = error(ErrorMsg)
    ).

:- pred spawn_native_joinable_2(
    pred(joinable_thread(T), T, io, io)::in(pred(in, out, di, uo) is cc_multi),
    uint::in, mutvar(T)::in, bool::out, thread_handle::out, string::out,
    io::di, io::uo) is cc_multi.

:- pragma foreign_proc("C",
    spawn_native_joinable_2(Goal::in(pred(in, out, di, uo) is cc_multi),
        MinStackSize::in, OutputMutvar::in,
        Success::out, ThreadHandle::out, ErrorMsg::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MR_THREAD_SAFE
    MR_String   thread_desc;

    Success = ML_create_exclusive_thread(TypeInfo_for_T, Goal,
        MinStackSize, OutputMutvar, &thread_desc, &ThreadHandle, &ErrorMsg,
        MR_ALLOC_ID);
#else
    Success = MR_FALSE;
    ThreadHandle = 0;
    ErrorMsg = MR_make_string_const(
        ""Cannot create joinable thread in this grade."");
#endif
").

:- pragma foreign_proc("C#",
    spawn_native_joinable_2(Goal::in(pred(in, out, di, uo) is cc_multi),
        _MinStackSize::in, OutputMutvar::in,
        Success::out, ThreadHandle::out, ErrorMsg::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    try {
        object[] thread_locals = runtime.ThreadLocalMutables.clone();
        RunGoalJoinable rg = new RunGoalJoinable(TypeInfo_for_T, Goal,
            thread_locals, OutputMutvar);
        System.Threading.Thread thread = new System.Threading.Thread(
            new System.Threading.ThreadStart(rg.run));
        rg.setThreadHandle(thread);
        thread.Start();

        Success = mr_bool.YES;
        ThreadHandle = thread;
        ErrorMsg = """";
    } catch (System.SystemException e) {
        // This includes System.Threading.ThreadStartException.
        // SystemException has been seen with mono.
        Success = mr_bool.NO;
        ThreadHandle = null;
        ErrorMsg = e.Message;
    }
").

:- pragma foreign_proc("Java",
    spawn_native_joinable_2(Goal::in(pred(in, out, di, uo) is cc_multi),
        _MinStackSize::in, OutputMutvar::in,
        Success::out, ThreadHandle::out, ErrorMsg::out, _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    try {
        RunGoalJoinable rg = new RunGoalJoinable(TypeInfo_for_T,
            (Object[]) Goal, OutputMutvar);
        Task task = new Task(rg);
        MercuryThread mt = JavaInternal.getThreadPool()
            .createExclusiveThread(task);
        rg.setThreadHandle(mt);
        mt.start();

        Success = bool.YES;
        ThreadHandle = mt;
        ErrorMsg = """";
    } catch (java.lang.SecurityException e) {
        Success = bool.NO;
        ThreadHandle = null;
        ErrorMsg = e.getMessage();
    } catch (java.lang.OutOfMemoryError e) {
        Success = bool.NO;
        ThreadHandle = null;
        ErrorMsg = e.getMessage();
    }
    if (Success == bool.NO && ErrorMsg == null) {
        ErrorMsg = ""Unable to create new native thread."";
    }
").

%---------------------------------------------------------------------------%

join_thread(Thread, Res, !IO) :-
    Thread = joinable_thread(ThreadHandle, OutputMutvar),
    promise_pure (
        join_thread_2(ThreadHandle, Success, ErrorMsg, !IO),
        (
            Success = yes,
            impure get_mutvar(OutputMutvar, Output),
            Res0 = ok(Output)
        ;
            Success = no,
            Res0 = error(ErrorMsg)
        )
    ),
    cc_multi_equal(Res0, Res).

:- pred join_thread_2(thread_handle::in, bool::out, string::out,
    io::di, io::uo) is det.

:- pragma foreign_proc("C",
    join_thread_2(ThreadHandle::in, Success::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, tabled_for_io,
        may_not_duplicate],
"
#ifdef MR_THREAD_SAFE
    int     err;
    char    errbuf[MR_STRERROR_BUF_SIZE];

    err = pthread_join(ThreadHandle, NULL);
    if (err == 0) {
        Success = MR_YES;
        ErrorMsg = MR_make_string_const("""");
    } else {
        Success = MR_NO;
        ErrorMsg = MR_make_string(MR_ALLOC_ID, ""pthread_join failed: %s"",
            MR_strerror(err, errbuf, sizeof(errbuf)));
    }
#else
    Success = MR_NO;
    ErrorMsg = MR_make_string_const(
        ""Native threads are not supported in this grade."");
#endif
").

:- pragma foreign_proc("C#",
    join_thread_2(ThreadHandle::in, Success::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    try {
        ThreadHandle.Join();
        Success = mr_bool.YES;
        ErrorMsg = """";
    } catch (System.SystemException e) {
        Success = mr_bool.NO;
        ErrorMsg = e.Message;
    }
").

:- pragma foreign_proc("Java",
    join_thread_2(ThreadHandle::in, Success::out, ErrorMsg::out,
        _IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    try {
        ThreadHandle.join();
        Success = bool.YES;
        ErrorMsg = """";
    } catch (java.lang.InterruptedException e) {
        Success = bool.NO;
        ErrorMsg = e.getMessage();
    }
").

%---------------------------------------------------------------------------%

:- pragma no_inline(pred(yield/2)).
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
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    System.Threading.Thread.Yield();
").

:- pragma foreign_proc("Java",
    yield(_IO0::di, _IO::uo),
    [promise_pure, will_not_call_mercury, thread_safe, may_not_duplicate],
"
    java.lang.Thread.yield();
").

yield(!IO).

%---------------------------------------------------------------------------%
%
% Low-level C implementation.
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
        // Call the closure placed the top of the stack.
        MR_r1 = MR_stackvar(1); // Goal
        MR_r2 = MR_stackvar(2); // ThreadDesc
        MR_decr_sp(2);
        MR_noprof_call(MR_ENTRY(mercury__do_call_closure_1),
            MR_LABEL(mercury__thread__spawn_end_thread));
    }
    MR_define_label(mercury__thread__spawn_end_thread);
    {
        ML_decr_thread_barrier_count();

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

    // Forward decls to suppress gcc warnings.
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
        // No types to register.
    }

    #ifdef  MR_DEEP_PROFILING
    void mercury_sys_init_thread_modules_write_out_proc_statics(FILE *deep_fp,
        FILE *procrep_fp)
    {
        // No proc_statics to write out.
    }
    #endif
").

%---------------------------------------------------------------------------%
%
% High-level C and low-level C exclusive threads.
%

:- pragma foreign_decl("C", local, "
#if defined(MR_THREAD_SAFE)

#include  <pthread.h>

static MR_bool ML_create_exclusive_thread(MR_Word typeinfo_for_T, MR_Word goal,
                size_t min_stack_size, MR_Word joinable_thread_mutvar,
                MR_String *thread_desc, pthread_t *thread_handle,
                MR_String *error_msg, MR_AllocSiteInfoPtr alloc_id);
static void   *ML_exclusive_thread_wrapper(void *arg);

typedef struct ML_ThreadWrapperArgs ML_ThreadWrapperArgs;
struct ML_ThreadWrapperArgs {
    MercuryLock         mutex;
    MercuryCond         cond;
    MR_Word             typeinfo_for_T;
    MR_Word             goal;
    MR_ThreadLocalMuts  *thread_local_mutables;
    MR_Word             joinable_thread_mutvar; // 0 for detached thread
    MR_Integer          thread_state;
    MR_String           thread_desc;
};

enum {
    ML_THREAD_NOT_READY,
    ML_THREAD_READY,
    ML_THREAD_START_ERROR
};

#endif // MR_THREAD_SAFE
").

:- pragma foreign_code("C", "
#if defined(MR_THREAD_SAFE)
static MR_bool
ML_create_exclusive_thread(MR_Word typeinfo_for_T, MR_Word goal,
    size_t min_stack_size, MR_Word joinable_thread_mutvar,
    MR_String *thread_desc, pthread_t *thread_handle,
    MR_String *error_msg, MR_AllocSiteInfoPtr alloc_id)
{
    ML_ThreadWrapperArgs    args;
    pthread_t               thread;
    pthread_attr_t          attrs;
    int                     err;
    char                    errbuf[MR_STRERROR_BUF_SIZE];

    *thread_desc = MR_make_string_const("""");
    *thread_handle = MR_null_thread();
    *error_msg = MR_make_string_const("""");

    ML_incr_thread_barrier_count();

    // The obvious synchronisation object to use here is a semaphore,
    // but glibc < 2.21 had a bug which could result in sem_post reading
    // from a semaphore after (in another thread) sem_wait returns and
    // destroys the semaphore.
    // <https://sourceware.org/bugzilla/show_bug.cgi?id=12674>

    pthread_mutex_init(&args.mutex, MR_MUTEX_ATTR);
    pthread_cond_init(&args.cond, MR_COND_ATTR);
    args.typeinfo_for_T = typeinfo_for_T;
    args.goal = goal;
    args.thread_local_mutables =
        MR_clone_thread_local_mutables(MR_THREAD_LOCAL_MUTABLES);
    args.joinable_thread_mutvar = joinable_thread_mutvar;
    // These fields will be updated by the newly created thread.
    args.thread_state = ML_THREAD_NOT_READY;
    args.thread_desc = NULL;

    pthread_attr_init(&attrs);
    if (joinable_thread_mutvar == 0) {
        err = pthread_attr_setdetachstate(&attrs, PTHREAD_CREATE_DETACHED);
        if (err != 0) {
            *error_msg = MR_make_string(alloc_id,
                ""pthread_attr_setdetachstate failed: %s"",
                MR_strerror(errno, errbuf, sizeof(errbuf)));
            goto failed_to_create_thread;
        }
    }
    if (min_stack_size > 0) {
        err = pthread_attr_setstacksize(&attrs, min_stack_size);
        if (err != 0) {
            *error_msg = MR_make_string(alloc_id,
                ""pthread_attr_setstacksize failed: %s"",
                MR_strerror(errno, errbuf, sizeof(errbuf)));
            goto failed_to_create_thread;
        }
    }

    err = pthread_create(&thread, &attrs, ML_exclusive_thread_wrapper, &args);
    if (err != 0) {
        *error_msg = MR_make_string(alloc_id, ""pthread_create failed: %s"",
            MR_strerror(errno, errbuf, sizeof(errbuf)));
        goto failed_to_create_thread;
    }

    MR_LOCK(&args.mutex, ""ML_create_exclusive_thread"");
    while (args.thread_state == ML_THREAD_NOT_READY) {
        err = MR_COND_WAIT(&args.cond, &args.mutex,
            ""ML_create_exclusive_thread"");
        // EINTR should not be possible, but it has happened before.
        if (err != 0 && errno != EINTR) {
            MR_fatal_error(
                ""ML_create_exclusive_thread: MR_COND_WAIT error: %s"",
                MR_strerror(errno, errbuf, sizeof(errbuf)));
        }
    }
    MR_UNLOCK(&args.mutex, ""ML_create_exclusive_thread"");

    if (args.thread_state == ML_THREAD_START_ERROR) {
        *error_msg =
            MR_make_string_const(""Error setting up engine for thread."");
    }

failed_to_create_thread:
    pthread_attr_destroy(&attrs);
    pthread_cond_destroy(&args.cond);
    pthread_mutex_destroy(&args.mutex);

    if (args.thread_state == ML_THREAD_READY) {
        *thread_desc = args.thread_desc;
        *thread_handle = thread;
        return MR_TRUE;
    }

    ML_decr_thread_barrier_count();
    return MR_FALSE;
}

static void *ML_exclusive_thread_wrapper(void *arg)
{
    ML_ThreadWrapperArgs    *args = arg;
    MR_Word                 typeinfo_for_T;
    MR_Word                 goal;
    MR_Word                 joinable_thread_mutvar;
    MR_String               thread_desc;
    pthread_t               thread_handle;

    if (MR_init_thread(MR_use_now) == MR_FALSE) {
        MR_LOCK(&args->mutex, ""ML_exclusive_thread_wrapper"");
        args->thread_state = ML_THREAD_START_ERROR;
        MR_COND_SIGNAL(&args->cond, ""ML_exclusive_thread_wrapper"");
        MR_UNLOCK(&args->mutex, ""ML_exclusive_thread_wrapper"");
        return NULL;
    }

    // Set the context to have the current engine as its exclusive engine.
    MR_assert(MR_ENGINE(MR_eng_this_context) != NULL);
    MR_ENGINE(MR_eng_this_context)->MR_ctxt_exclusive_engine =
        MR_ENGINE(MR_eng_id);

    MR_assert(MR_THREAD_LOCAL_MUTABLES == NULL);
    MR_SET_THREAD_LOCAL_MUTABLES(args->thread_local_mutables);

    // Take a copy of args fields.
    typeinfo_for_T = args->typeinfo_for_T;
    goal = args->goal;
    joinable_thread_mutvar = args->joinable_thread_mutvar;

    thread_desc = MR_make_string(MR_ALLOC_SITE_RUNTIME,
        ""%"" MR_INTEGER_LENGTH_MODIFIER ""x"", MR_SELF_THREAD_ID);
    thread_handle = pthread_self();

    // Tell the 'parent' we are ready, passing back a thread descriptor and
    // thread handle.
    MR_LOCK(&args->mutex, ""ML_exclusive_thread_wrapper"");
    args->thread_state = ML_THREAD_READY;
    args->thread_desc = thread_desc;
    MR_COND_SIGNAL(&args->cond, ""ML_exclusive_thread_wrapper"");
    MR_UNLOCK(&args->mutex, ""ML_exclusive_thread_wrapper"");
    // We must not dereference args after this point.

    if (joinable_thread_mutvar == 0) {
        ML_call_back_to_mercury_detached_cc_multi(goal, thread_desc);
    } else {
        ML_call_back_to_mercury_joinable_cc_multi(typeinfo_for_T, goal,
            thread_handle, joinable_thread_mutvar);
    }

    MR_finalize_thread_engine();

    ML_decr_thread_barrier_count();

    return NULL;
}
#endif // MR_THREAD_SAFE
").

:- pred call_back_to_mercury_detached(
    pred(thread, io, io)::in(pred(in, di, uo) is cc_multi),
    thread_desc::in, io::di, io::uo) is cc_multi.
:- pragma foreign_export("C",
    call_back_to_mercury_detached(in(pred(in, di, uo) is cc_multi),
    in, di, uo), "ML_call_back_to_mercury_detached_cc_multi").
:- pragma foreign_export("C#",
    call_back_to_mercury_detached(in(pred(in, di, uo) is cc_multi),
    in, di, uo), "ML_call_back_to_mercury_detached_cc_multi").
:- pragma foreign_export("Java",
    call_back_to_mercury_detached(in(pred(in, di, uo) is cc_multi),
    in, di, uo), "ML_call_back_to_mercury_detached_cc_multi").
:- pragma no_inline(pred(call_back_to_mercury_detached/4)).

call_back_to_mercury_detached(Goal, ThreadDesc, !IO) :-
    Thread = detached_thread(ThreadDesc),
    Goal(Thread, !IO).

:- pred call_back_to_mercury_joinable(
    pred(joinable_thread(T), T, io, io)::in(pred(in, out, di, uo) is cc_multi),
    thread_handle::in, mutvar(T)::in, io::di, io::uo) is cc_multi.
:- pragma foreign_export("C",
    call_back_to_mercury_joinable(in(pred(in, out, di, uo) is cc_multi),
    in, in, di, uo), "ML_call_back_to_mercury_joinable_cc_multi").
:- pragma foreign_export("C#",
    call_back_to_mercury_joinable(in(pred(in, out, di, uo) is cc_multi),
    in, in, di, uo), "ML_call_back_to_mercury_joinable_cc_multi").
:- pragma foreign_export("Java",
    call_back_to_mercury_joinable(in(pred(in, out, di, uo) is cc_multi),
    in, in, di, uo), "ML_call_back_to_mercury_joinable_cc_multi").
:- pragma no_inline(pred(call_back_to_mercury_joinable/5)).

call_back_to_mercury_joinable(Goal, ThreadHandle, OutputMutvar, !IO) :-
    Thread = joinable_thread(ThreadHandle, OutputMutvar),
    promise_pure (
        Goal(Thread, Output, !IO),
        % Store a reference to the output term in a mutvar that is in turn
        % referenced from a joinable_thread term. If we simply returned the
        % output term as the return value of the pthread_create start routine
        % (ML_exclusive_thread_wrapper), it might be possible that the last
        % reference to the term resides only in some GC-inaccessible memory
        % in the pthread implementation, and therefore could be collected
        % before join_thread retrieves the value.
        %
        % The C# or Java thread APIs do not support returning a value from a
        % joined thread anyway.
        impure set_mutvar(OutputMutvar, Output)
    ).

%---------------------------------------------------------------------------%

:- pragma foreign_decl("C", local,
"
#if defined(MR_THREAD_SAFE) || !defined(MR_HIGHLEVEL_CODE)
  static void ML_incr_thread_barrier_count(void);
  static void ML_decr_thread_barrier_count(void);
#endif
").

:- pragma foreign_code("C",
"
#if defined(MR_THREAD_SAFE) || !defined(MR_HIGHLEVEL_CODE)

  static void ML_incr_thread_barrier_count(void)
  {
    MR_LOCK(&MR_thread_barrier_lock, ""ML_incr_thread_barrier_count"");
    MR_thread_barrier_count++;
    MR_UNLOCK(&MR_thread_barrier_lock, ""ML_incr_thread_barrier_count"");
  }

  static void ML_decr_thread_barrier_count(void)
  {
    MR_LOCK(&MR_thread_barrier_lock, ""ML_decr_thread_barrier_count"");
    MR_thread_barrier_count--;
  #ifdef MR_HIGHLEVEL_CODE
    if (MR_thread_barrier_count == 0) {
        MR_COND_SIGNAL(&MR_thread_barrier_cond,
            ""ML_decr_thread_barrier_count"");
    }
  #else
    if (MR_thread_barrier_count == 0) {
        // If this is the last spawned context to terminate and the
        // main context was just waiting on us in order to terminate,
        // then reschedule the main context.

        if (MR_thread_barrier_context) {
            MR_schedule_context(MR_thread_barrier_context);
            MR_thread_barrier_context = NULL;
        }
    }
  #endif
    MR_UNLOCK(&MR_thread_barrier_lock, ""ML_decr_thread_barrier_count"");
  }

#endif // MR_THREAD_SAFE || !MR_HIGHLEVEL_CODE
").

%---------------------------------------------------------------------------%

:- pragma foreign_code("C#", "
private class RunGoalDetached {
    private object[]    goal;
    private object[]    thread_local_mutables;
    private string      thread_desc;

    internal RunGoalDetached(object[] goal, object[] tlmuts)
    {
        this.goal = goal;
        this.thread_local_mutables = tlmuts;
        this.thread_desc = null;
    }

    internal void setThreadDesc(string thread_desc)
    {
        this.thread_desc = thread_desc;
    }

    internal void run()
    {
        runtime.ThreadLocalMutables.set_array(thread_local_mutables);
        thread.ML_call_back_to_mercury_detached_cc_multi(goal, thread_desc);
    }
}

private class RunGoalJoinable {
    private runtime.TypeInfo_Struct typeinfo_for_T;
    private object[]                goal;
    private object[]                thread_local_mutables;
    private object[]                output_mutvar;
    private System.Threading.Thread thread_handle;

    internal RunGoalJoinable(runtime.TypeInfo_Struct typeinfo_for_T,
        object[] goal, object[] tlmuts, object[] output_mutvar)
    {
        this.typeinfo_for_T = typeinfo_for_T;
        this.goal = goal;
        this.thread_local_mutables = tlmuts;
        this.output_mutvar = output_mutvar;
        this.thread_handle = null;
    }

    internal void setThreadHandle(System.Threading.Thread thread_handle)
    {
        this.thread_handle = thread_handle;
    }

    internal void run()
    {
        runtime.ThreadLocalMutables.set_array(thread_local_mutables);
        thread.ML_call_back_to_mercury_joinable_cc_multi(typeinfo_for_T, goal,
            thread_handle, output_mutvar);
    }
}
").

:- pragma foreign_code("Java", "
public static class RunGoalDetached implements Runnable {
    private final Object[]  goal;
    private String          thread_desc;

    private RunGoalDetached(Object[] goal)
    {
        this.goal = goal;
        this.thread_desc = null;
    }

    private void setThreadDesc(String thread_desc)
    {
        this.thread_desc = thread_desc;
    }

    public void run()
    {
        thread.ML_call_back_to_mercury_detached_cc_multi(goal, thread_desc);
    }
}

public static class RunGoalJoinable implements Runnable {
    private final jmercury.runtime.TypeInfo_Struct typeinfo_for_T;
    private final Object[]      goal;
    private final mutvar.Mutvar output_mutvar;
    private MercuryThread       thread_handle;

    private RunGoalJoinable(jmercury.runtime.TypeInfo_Struct typeinfo_for_T,
        Object[] goal, mutvar.Mutvar output_mutvar)
    {
        this.typeinfo_for_T = typeinfo_for_T;
        this.goal = goal;
        this.output_mutvar = output_mutvar;
        this.thread_handle = null;
    }

    private void setThreadHandle(MercuryThread thread_handle)
    {
        this.thread_handle = thread_handle;
    }

    public void run()
    {
        thread.ML_call_back_to_mercury_joinable_cc_multi(typeinfo_for_T, goal,
            thread_handle, output_mutvar);
    }
}
").

%---------------------------------------------------------------------------%

num_processors(MaybeProcs, !IO) :-
    num_processors(Procs, Success, !IO),
    (
        Success = yes,
        MaybeProcs = yes(Procs)
    ;
        Success = no,
        MaybeProcs = no
    ).

:- pred num_processors(int::out, bool::out, io::di, io::uo) is det.

:- pragma foreign_proc("C",
    num_processors(Procs::out, Success::out, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury,
     will_not_throw_exception, tabled_for_io],
"
#ifdef MR_THREAD_SAFE
    // MR_get_num_processors() takes the global lock.
    Procs = MR_get_num_processors();
#else
    Procs = 0;
#endif
    Success = (Procs > 0) ? MR_YES : MR_NO;
").

:- pragma foreign_proc("C#",
    num_processors(Procs::out, Success::out, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury,
     will_not_throw_exception],
"
    Procs = System.Environment.ProcessorCount;
    Success = mr_bool.YES;
").

:- pragma foreign_proc("Java",
    num_processors(Procs::out, Success::out, _IO0::di, _IO::uo),
    [promise_pure, thread_safe, will_not_call_mercury,
     will_not_throw_exception],
"
    Procs = Runtime.getRuntime().availableProcessors();
    Success = bool.YES;
").

%---------------------------------------------------------------------------%
:- end_module thread.
%---------------------------------------------------------------------------%
