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
% module in parallel grades.  It is intended for the builtin procedures that
% the compiler generates implicit calls to when implementing parallel
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
    typedef struct MR_Future MR_Future;

#ifdef MR_THREAD_SAFE
# ifdef MR_HAVE_SEMAPHORE_H
    /* POSIX 1003.1b semaphores available. */
    #include <semaphore.h>

    struct MR_Future {
        sem_t   semaphore;
        MR_Word value;
    };
# else /* !MR_HAVE_SEMAPHORE_H */
    /* Use POSIX thread mutexes and condition variables. */
    #include <pthread.h>

    struct MR_Future {
        pthread_mutex_t mutex;
        pthread_cond_t cond;
        MR_Word value;
    };
# endif /* !MR_HAVE_SEMAPHORE_H */
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
#ifdef MR_THREAD_SAFE
# ifdef MR_HAVE_SEMAPHORE_H
    Future = MR_GC_NEW(MR_Future);
    sem_init(&Future->semaphore, MR_NO, 0);
    Future->value = 0;
# else
    Future = MR_GC_NEW(MR_Future);
    pthread_mutex_init(&Future->mutex, NULL);
    pthread_cond_init(&Future->cond, NULL);
    Future->value = 0;
# endif
#endif
").

:- pragma foreign_proc("C",
    wait(Future::in, Value::out),
    [will_not_call_mercury, promise_pure, thread_safe, will_not_modify_trail],
"
#ifdef MR_THREAD_SAFE
# ifdef MR_HAVE_SEMAPHORE_H
    sem_wait(&Future->semaphore);
    sem_post(&Future->semaphore);
    Value = Future->value;
# else
    pthread_mutex_lock(&Future->mutex);
    while (!Future->pass) {
        pthread_cond_wait(&Future->cond, &Future->mutex);
    }
    Value = Future->value;
    pthread_mutex_unlock(&Future->mutex);
# endif
#endif
").

:- pragma foreign_proc("C",
    signal(Future::in, Value::in),
    [will_not_call_mercury, thread_safe, will_not_modify_trail],
"
#ifdef MR_THREAD_SAFE
# ifdef MR_HAVE_SEMAPHORE_H
    Future->value = Value;
    sem_post(&Future->semaphore);
# else
    pthread_mutex_lock(&Future->mutex);
    Value = Future->value;
    pthread_cond_broadcast(&Future->cond);
    pthread_mutex_unlock(&Future->mutex);
# endif
#endif
").

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
