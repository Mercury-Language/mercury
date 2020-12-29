%---------------------------------------------------------------------------%
% vim: ft=mercury ts=8 sw=4 sts=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2006-2011 The University of Melbourne.
% Copyright (C) 2014-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: par_builtin.m.
% Main authors: wangp, pbone.
% Stability: low.
%
% This file is automatically imported, as if via `use_module', into every
% module in lowlevel parallel grades. It holds the builtin procedures
% that the compiler generates implicit calls to when implementing parallel
% conjunctions.
%
% This module is a private part of the Mercury implementation; user modules
% should never explicitly import this module. The interface for this module
% does not get included in the Mercury library reference manual.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

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
    % wait_future/2 doesn't actually have a side effect. However once it has
    % returned, get_future/2 is guaranteed to be safe. Therefore it must be
    % impure to prevent it from being optimized away, especially in (valid)
    % cases where its output occurs only once in a procedure.
    %
:- impure pred wait_future(future(T)::in, T::out) is det.

    % get_future(Future, Var):
    %
    % Like wait_future, but assumes that the future has been signalled already.
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
%
% These types and predicates are used by the loop control transformation.
%

:- type loop_control.

    % Create a loop control structure.
    % For documentation, see MR_lc_create in mercury_par_builtin.h.
    %
:- pred lc_create(int::in, loop_control::out) is det.

    % Finish a loop and finalize a loop control structure.
    % For documentation, see MR_lc_finish in mercury_par_builtin.h.
    %
:- impure pred lc_finish(loop_control::in) is det.

    % Allocate a free slot from the loop control structure and return it.
    % For documentation, see MR_lc_try_get_free_slot in mercury_par_builtin.h
    % This call fails if there is no free slot available.
    %
:- impure pred lc_free_slot(loop_control::in, int::out) is semidet.

    % Allocate a free slot from the loop control structure and return it.
    % This call blocks the context until a free slot is available.
    %
:- impure pred lc_wait_free_slot(loop_control::in, int::out) is det.

    % Finish one iteration of the loop. This call does not return.
    % For documentation, see MR_lc_join_and_terminate in mercury_par_builtin.h.
    %
:- impure pred lc_join_and_terminate(loop_control::in, int::in) is det.

    % Get the default number of contexts to use for loop control.
    %
:- impure pred lc_default_num_contexts(int::out) is det.

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

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("C", future(T), "MR_Future *",
    [can_pass_as_mercury_type]).

    % Placeholders only.
:- pragma foreign_type("C#", future(T), "object").
:- pragma foreign_type("Java", future(T), "java.lang.Object").

%---------------------------------------------------------------------------%

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
    // All uses of this predicate should override the body.
    MR_fatal_error(""evaluate_parallelism_condition called"");
").

%---------------------------------------------------------------------------%

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

        // Restore the address of the future after resuming.
        Future = (MR_Future *) MR_sv(1);
        MR_decr_sp(1);

        assert(Future->MR_fut_signalled);

        // Return to the caller of par_builtin.wait.
        MR_r1 = Future->MR_fut_value;
        MR_proceed();
    }
    MR_END_MODULE

#endif

    // Forward decls to suppress gcc warnings.
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
        // No types to register.
    }

    #ifdef  MR_DEEP_PROFILING
    void mercury_sys_init_par_builtin_modules_write_out_proc_statics(
        FILE *deep_fp, FILE *procrep_fp)
    {
        // No proc_statics to write out.
    }
    #endif
").

%---------------------------------------------------------------------------%

:- pragma foreign_type("C", loop_control, "MR_LoopControl *",
    [can_pass_as_mercury_type]).

    % Placeholders only.
:- pragma foreign_type("C#", loop_control, "object").
:- pragma foreign_type("Java", loop_control, "java.lang.Object").

:- pragma foreign_proc("C",
    lc_create(NumWorkers::in, LC::out),
    [will_not_call_mercury, will_not_throw_exception, thread_safe,
        promise_pure],
"
#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ)
    LC = MR_lc_create(NumWorkers);
#else
    MR_fatal_error(""lc_create is unavailable in this grade"");
#endif
").

% IMPORTANT: any changes or additions to external predicates should be
% reflected in the definition of pred_is_external in mdbcomp/
% program_representation.m. The debugger needs to know what predicates
% are defined externally, so that it knows not to expect events for those
% predicates.
:- pragma external_pred(lc_finish/1).
:- pragma external_pred(lc_wait_free_slot/2).

:- pragma foreign_code("C",
"

void mercury_sys_init_lc_init(void);
void mercury_sys_init_lc_init_type_tables(void);
#ifdef  MR_DEEP_PROFILING
void mercury_sys_init_lc_write_out_proc_statics(FILE *deep_fp,
    FILE *procrep_fp);
#endif


#ifdef MR_HIGHLEVEL_CODE

void MR_CALL
mercury__par_builtin__lc_finish_1_p_0(MR_Box lc)
{
    MR_fatal_error(""lc_finish is unavailable with --highlevel-code"");
}

void MR_CALL
mercury__par_builtin__lc_wait_free_slot_2_p_0(MR_Box lc, MR_Integer *slot)
{
    MR_fatal_error(""lc_wait_free_slot is unavailable with --highlevel-code"");
}

#else // ! MR_HIGHLEVEL_CODE

MR_def_extern_entry(par_builtin__lc_finish_1_0)
MR_def_extern_entry(par_builtin__lc_wait_free_slot_2_0)

MR_decl_label1(par_builtin__lc_finish_1_0, 1)

MR_BEGIN_MODULE(par_builtin_module_lc_finish)
    MR_init_entry1(par_builtin__lc_finish_1_0);
    MR_INIT_PROC_LAYOUT_ADDR(mercury__par_builtin__lc_finish_1_0);
    MR_init_label1(par_builtin__lc_finish_1_0,1);
MR_BEGIN_CODE

#ifdef MR_maybe_local_thread_engine_base
    #undef MR_maybe_local_thread_engine_base
    #define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif

MR_define_entry(mercury__par_builtin__lc_finish_1_0)
    MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE

    MR_incr_sp(1);
    MR_sv(1) = MR_r1; // LC

#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ)
    {
        MR_LoopControl  *LC;

        LC = (MR_LoopControl *) MR_r1;
        MR_lc_finish_part1(LC, MR_LABEL_AP(par_builtin__lc_finish_1_0_i1));
    }
#else
    MR_fatal_error(""lc_finish is unavailable in this grade"");
#endif

MR_def_label(par_builtin__lc_finish_1_0,1)
    MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE

#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ)
    {
        MR_LoopControl  *LC;

        LC = (MR_LoopControl *) MR_sv(1);
        MR_lc_finish_part2(LC);
    }
#endif

    MR_decr_sp(1);
    MR_proceed();

#ifdef MR_maybe_local_thread_engine_base
    #undef MR_maybe_local_thread_engine_base
    #define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

MR_decl_label1(par_builtin__lc_wait_free_slot_2_0, 1)

MR_BEGIN_MODULE(par_builtin_module_lc_wait_free_slot)
    MR_init_entry1(par_builtin__lc_wait_free_slot_2_0);
    MR_INIT_PROC_LAYOUT_ADDR(mercury__par_builtin__lc_wait_free_slot_2_0);
    MR_init_label1(par_builtin__lc_wait_free_slot_2_0,1);
MR_BEGIN_CODE

#ifdef MR_maybe_local_thread_engine_base
    #undef MR_maybe_local_thread_engine_base
    #define MR_maybe_local_thread_engine_base MR_local_thread_engine_base
#endif

MR_define_entry(mercury__par_builtin__lc_wait_free_slot_2_0)
    MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE

    MR_incr_sp(1);
    MR_sv(1) = MR_r1; // LC
    // LC must be saved to the stack so that we can resume from the label below
    // and retrieve it.

MR_def_label(par_builtin__lc_wait_free_slot_2_0,1)
    MR_MAYBE_INIT_LOCAL_THREAD_ENGINE_BASE

#if defined(MR_LL_PARALLEL_CONJ)
    {
        MR_LoopControl  *lc;
        MR_Unsigned     lcs_idx;

        lc = (MR_LoopControl *) MR_sv(1);
        MR_lc_wait_free_slot(lc, lcs_idx,
            MR_LABEL_AP(par_builtin__lc_wait_free_slot_2_0_i1));
        MR_r1 = (MR_Word)lcs_idx;
    }
#else
    MR_fatal_error(""lc_wait_free_slot is unavailable in this grade"");
#endif

    MR_decr_sp(1);
    MR_proceed();

#ifdef MR_maybe_local_thread_engine_base
    #undef MR_maybe_local_thread_engine_base
    #define MR_maybe_local_thread_engine_base MR_thread_engine_base
#endif
MR_END_MODULE

#endif // ! MR_HIGHLEVEL_CODE

// Module initialization.
/*
INIT mercury_sys_init_lc
*/

void
mercury_sys_init_lc_init(void)
{
#ifndef MR_HIGHLEVEL_CODE
    par_builtin_module_lc_finish();
    par_builtin_module_lc_wait_free_slot();
#endif
}

void
mercury_sys_init_lc_init_type_tables(void)
{
    // No types to register.
}

#ifdef  MR_DEEP_PROFILING
void
mercury_sys_init_lc_write_out_proc_statics(FILE *deep_fp,
    FILE *procrep_fp)
{
    // The deep profiler shouldn't notice loop control predicates.
}
#endif

").

:- pragma foreign_proc("C",
    lc_free_slot(LC::in, LCS::out),
    [will_not_call_mercury, will_not_throw_exception, thread_safe],
"
#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ)
    {
        MR_Unsigned LCS_0;
        SUCCESS_INDICATOR = MR_lc_try_get_free_slot(LC, &LCS_0);
        LCS = (MR_Integer)LCS_0;
    }
#else
    MR_fatal_error(""lc_free_slot is unavailable in this grade"");
#endif
").

:- pragma foreign_proc("C",
    lc_join_and_terminate(LC::in, LCS::in),
    [will_not_call_mercury, will_not_throw_exception, thread_safe],
"
#if defined(MR_THREAD_SAFE) && defined(MR_LL_PARALLEL_CONJ)
    MR_lc_join_and_terminate(LC, LCS);
#else
    MR_fatal_error(""lc_join_and_terminate is unavailable in this grade"");
#endif
").

:- pragma foreign_proc("C",
    lc_default_num_contexts(NumContexts::out),
    [will_not_call_mercury, will_not_throw_exception, thread_safe],
"
    NumContexts = MR_num_contexts_per_loop_control;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    par_cond_close_stats_file(_IO0::di, _IO::uo),
    [will_not_call_mercury, thread_safe, promise_pure, tabled_for_io],
"
#if defined(MR_LL_PARALLEL_CONJ) && \
        defined(MR_DEBUG_RUNTIME_GRANULARITY_CONTROL)
    MR_write_out_conditional_parallelism_log();
#else
    MR_fatal_error(""par_cond_close_stats_file is unavailable in this grade"");
#endif
").

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
