%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: stream.m.
% Main author: conway.
% Stability: medium.
%
% This module implements a simple concurrent data-stream.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module stream.
:- interface.

:- import_module io.

%-----------------------------------------------------------------------------%

:- type stream(T).

:- type stream.result(T)
    --->    end
    ;       error(string)
    ;       ok(T).

    % new(Stream, !IO) creates a new data stream `Stream'.
    %
:- pred new(stream(T)::out, io::di, io::uo) is det.

    % get(Stream, Result, !IO) blocks until a message appears
    % on the data stream `Stream'. When a message arrives, `Result' is
    % bound to the value of the message.
    %
:- pred get(stream(T)::in, stream.result(T)::out, io::di, io::uo) is det.

    % put(Stream, Thing, !IO) adds `Thing' to the end of the stream
    % `Stream', waking a call to get/4 if necessary.
    %
:- pred put(stream(T)::in, T::in, io::di, io::uo) is det.

    % end(Stream, !IO) puts an end-of-stream marker on the stream
    % `Stream', waking a call to get/4 if necessary.
    %
:- pred end(stream(T)::in, io::di, io::uo) is det.

    % error(Stream, !IO) puts an error message on the stream
    % `Stream', waking a call to get/4 if necessary.
    %
:- pred error(stream(T)::in, string::in, io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module global.
:- import_module semaphore.

:- import_module queue.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type stream(T)
    --->    stream(
                semaphore,
                global(stream0(T)),
                semaphore
            ).

:- type stream0(T) == queue(stream.result(T)).

new(Stream, !IO) :-
    queue.init(Queue),
    new(Queue, QueueGlob, !IO),
    new(Lock, !IO),
    signal(Lock, !IO),
    new(Semaphore, !IO),
    Stream = stream(Lock, QueueGlob, Semaphore).

put(Stream, Thing, !IO) :-
    Stream = stream(Lock, QueueGlob, Semaphore),
    wait(Lock, !IO),
    get(QueueGlob, Queue0, !IO),
    queue.put(Queue0, ok(Thing), Queue),
    set(QueueGlob, Queue, !IO),
    signal(Lock, !IO),
    signal(Semaphore, !IO).

end(Stream, !IO) :-
    Stream = stream(Lock, QueueGlob, Semaphore),
    wait(Lock, !IO),
    get(QueueGlob, Queue0, !IO),
    queue.put(Queue0, end, Queue),
    set(QueueGlob, Queue, !IO),
    signal(Lock, !IO),
    signal(Semaphore, !IO).

error(Stream, Msg, !IO) :-
    Stream = stream(Lock, QueueGlob, Semaphore),
    wait(Lock, !IO),
    get(QueueGlob, Queue0, !IO),
    queue.put(Queue0, error(Msg), Queue),
    set(QueueGlob, Queue, !IO),
    signal(Lock, !IO),
    signal(Semaphore, !IO).

get(Stream, Thing, !IO) :-
    Stream = stream(Lock, QueueGlob, Semaphore),
    wait(Semaphore, !IO),
    wait(Lock, !IO),
    get(QueueGlob, Queue0, !IO),
    ( queue.get(Queue0, Thing0, Queue) ->
        Thing = Thing0,
        set(QueueGlob, Queue, !IO)
    ;
        error("stream.get/4: queue and semaphore out of sync")
    ),
    signal(Lock, !IO).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

