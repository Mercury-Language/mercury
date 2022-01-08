%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014-2015, 2018 The Mercury Team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: thread.future.m.
% Authors: pbone.
% Stability: low.
%
% This module defines the data types future_io/1 and future/1 which are
% useful for parallel and concurrent programming.
%
% A future represents a value that might not exist yet.  A value for a
% future may be provided exactly once, but can be read any number of times.
% In these situations futures can be faster than mvars as their
% implementation is simpler: they need only one semaphore and they can avoid
% using it in some cases.
%
% There are two kinds of futures:
%
%   + future(T) is a value that will be evaluated by another thread.  The
%     function future/1 will spawn a new thread to evaluate its argument
%     whose result can be retrieved later by calling the function wait/1.
%     For example:
%
%       Future = future(SomeFunction),
%       ... do something in the meantime ...
%       Value = wait(Future).
%
%   + future_io(T) provides more flexibility, allowing the caller to control
%     the creation of the thread that provides its value.  It can be used
%     as follows:
%
%       First:
%           future(Future, !IO),
%
%       Then in a separate thread:
%           signal(Future, Value0, !IO),
%
%       Finally, in the original thread:
%           wait(Future, Value, !IO),
%
%     This is more flexible because the thread can be used to signal
%     multiple futures or do other things, but it requires the I/O state.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module thread.future.
:- interface.

%---------------------------------------------------------------------------%

    % future/1 represents a value that will be computed by another thread.
    %
:- type future(T).

    % Create a future which has the value that the argument, when evaluated,
    % will produce.  This function will create a thread to evaluate the
    % argument using spawn/3.
    %
    % If the argument throws an exception, that exception will be rethrown by
    % wait/1.
    %
:- func future((func) = T) = future(T).

    % Return the value of the future, blocking until the value is available.
    %
:- func wait(future(T)) = T.

%---------------------------------------------------------------------------%

    % future_io/1 represents a value that may not have been computed yet.
    % Future values are intended to be computed by separate threads (using
    % spawn/3).
    %
    % Generally in computer science and in some other languages this is
    % known as a promise.  We called it future_io because promise is a
    % reserved word in Mercury.
    %
:- type future_io(T).

    % Create a new empty future_io.
    %
:- pred init(future_io(T)::uo, io::di, io::uo) is det.

    % Provide a value for the future_io and signal any waiting threads.  Any
    % further calls to wait will return immediately.
    %
    % Calling signal multiple times will result in undefined behaviour.
    %
:- pred signal(future_io(T)::in, T::in, io::di, io::uo) is det.

    % Return the future_io's value, potentially blocking until it is
    % signaled.
    %
:- pred wait(future_io(T)::in, T::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
:- implementation.

:- import_module exception.
:- import_module thread.semaphore.
:- import_module mutvar.

%---------------------------------------------------------------------------%

:- type future(T)
    --->    future(future_io(ok_or_exception(T))).

:- type ok_or_exception(T)
    --->    ok(T)
    ;       some [E] exception(E).

future(Func) = Future :-
    promise_pure (
        impure init(FutureIO),
        Future = future(FutureIO),
        impure spawn_impure(run_future(Future, Func))
    ).

:- impure pred run_future(future(T), (func) = T).
:-        mode run_future(in, ((func) = out) is det) is cc_multi.

run_future(future(Future), Func) :-
    ( try []
        Result = apply(Func)
    then
        impure signal(Future, ok(Result))
    catch_any Exp ->
        impure signal(Future, 'new exception'(Exp))
    ).

wait(future(Future)) = Value :-
    wait(Future, Result),
    (
        Result = ok(Value)
    ;
        Result = exception(Exception),
        throw(Exception)
    ).

:- impure pred spawn_impure(impure (pred)).
:-        mode spawn_impure((pred) is cc_multi) is det.

spawn_impure(Task) :-
    impure make_io_state(IO0),
    promise_equivalent_solutions [IO] (
        spawn(spawn_impure_2(Task), IO0, IO)
    ),
    impure consume_io_state(IO).

:- pred spawn_impure_2(impure (pred), io, io).
:- mode spawn_impure_2((pred) is cc_multi, di, uo) is cc_multi.
:- pragma promise_pure(pred(spawn_impure_2/3)).

spawn_impure_2(Task, !IO) :-
    impure Task.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type future_io(T)
    --->    future_io(
                f_ready         :: mutvar(ready),
                % f_ready is used to optimistically avoid locking.  It is
                % also used to try to detect multiple calls to signal/2.

                f_wait          :: semaphore,
                f_value         :: mutvar(T)
            ).

:- type ready
    --->    ready
    ;       not_ready.

init(Future, !IO) :-
    promise_pure (
        impure init(Future)
    ).

:- impure pred init(future_io(T)::uo) is det.

init(future_io(Ready, Wait, Value)) :-
    impure new_mutvar(not_ready, Ready),
    impure semaphore.impure_init(Wait),
    impure new_mutvar0(Value).

%---------------------------------------------------------------------------%

signal(Future, Value, !IO) :-
    promise_pure (
        impure signal(Future, Value)
    ).

:- impure pred signal(future_io(T)::in, T::in) is det.

signal(future_io(MReady, Wait, MValue), Value) :-
    impure get_mutvar(MReady, Ready),
    (
        Ready = not_ready,
        impure set_mutvar(MValue, Value),
        % TODO: Implement signal_all.
        impure semaphore.impure_signal(Wait),
        % We must write MReady _after_ signaling the semaphore.  The signal
        % provides a memory barrier that ensures that the write to MReady
        % occurs after MValue.  This ensures that the optimisation in wait/4
        % will read the future consistently.
        impure set_mutvar(MReady, ready)
    ;
        Ready = ready,
        % It is possible that another thread has called signal/2 but we read
        % Ready before it wrote it, resulting in multiple calls to signal/2.
        % Therefore we do not guarantee that we will always detect multiple
        % calls and will not always throw this exception.
        error("Multiple calls to thread.future.signal/2")
    ).

%---------------------------------------------------------------------------%

wait(Future, Value, !IO) :-
    wait(Future, Value).

    % Wait is pure because it always returns the same value for the same
    % future (if it terminates).
    %
:- pred wait(future_io(T)::in, T::out) is det.

wait(Future, Value) :-
    promise_pure (
        Future = future_io(MReady, Wait, MValue),
        impure get_mutvar(MReady, Ready),
        (
            Ready = ready
            % No wait necessary
        ;
            Ready = not_ready,
            % We need to wait, this will probably block.
            impure semaphore.impure_wait(Wait),
            % Signal the semaphore to release the next waiting thread.
            impure semaphore.impure_signal(Wait)
        ),
        impure get_mutvar(MValue, Value)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

%
% Copied from exception.m
%

:- impure pred make_io_state(io::uo) is det.
:- pragma foreign_proc("C", make_io_state(_IO::uo),
    [will_not_call_mercury, thread_safe, will_not_modify_trail, no_sharing],
    "").
:- pragma foreign_proc("C#", make_io_state(_IO::uo),
    [will_not_call_mercury, thread_safe], "").
:- pragma foreign_proc("Java", make_io_state(_IO::uo),
    [will_not_call_mercury, thread_safe], "").

:- impure pred consume_io_state(io::di) is det.
:- pragma foreign_proc("C",
    consume_io_state(_IO::di),
    [will_not_call_mercury, thread_safe, no_sharing], "").
:- pragma foreign_proc("C#",
    consume_io_state(_IO::di),
    [will_not_call_mercury, thread_safe], "").
:- pragma foreign_proc("Java",
    consume_io_state(_IO::di),
    [will_not_call_mercury, thread_safe], "").

%---------------------------------------------------------------------------%
:- end_module thread.future.
%---------------------------------------------------------------------------%
