%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Main author: conway
% Stability: medium.
%
% This module implements a simple concurrent data-stream.
%
%---------------------------------------------------------------------------%
:- module stream.

:- interface.

:- import_module io.

:- type stream(T).

:- type stream__result(T)
	--->	end
	;	error(string)
	;	ok(T)
	.

	% new(Stream, IO0, IO) creates a new data stream `Stream'.
:- pred new(stream(T), io__state, io__state).
:- mode new(out, di, uo) is det.

	% get(Stream, Result, IO0, IO) blocks until a message appears
	% on the data stream `Stream'. When a message arrives, `Result' is
	% bound to the value of the message.
:- pred get(stream(T), stream__result(T), io__state, io__state).
:- mode get(in, out, di, uo) is det.

	% put(Stream, Thing, IO0, IO) adds `Thing' to the end of the stream
	% `Stream', waking a call to get/4 if necessary.
:- pred put(stream(T), T, io__state, io__state).
:- mode put(in, in, di, uo) is det.

	% end(Stream, IO0, IO) puts an end-of-stream marker on the stream
	% `Stream', waking a call to get/4 if necessary.
:- pred end(stream(T), io__state, io__state).
:- mode end(in, di, uo) is det.

	% error(Stream, IO0, IO) puts an error message on the stream
	% `Stream', waking a call to get/4 if necessary.
:- pred error(stream(T), string, io__state, io__state).
:- mode error(in, in, di, uo) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module queue, require.
:- import_module global, semaphore.

:- type stream(T)
	--->	stream(
			semaphore,
			global(stream0(T)),
			semaphore
		).

:- type stream0(T) ==	queue(stream__result(T)).

new(Stream) -->
	{ queue__init(Queue) },
	new(Queue, QueueGlob),
	new(Lock), signal(Lock),
	new(Semaphore),
	{ Stream = stream(Lock, QueueGlob, Semaphore) }.

put(Stream, Thing) -->
	{ Stream = stream(Lock, QueueGlob, Semaphore) },
	wait(Lock),
	get(QueueGlob, Queue0),
	{ queue__put(Queue0, ok(Thing), Queue) },
	set(QueueGlob, Queue),
	signal(Lock),
	signal(Semaphore).

end(Stream) -->
	{ Stream = stream(Lock, QueueGlob, Semaphore) },
	wait(Lock),
	get(QueueGlob, Queue0),
	{ queue__put(Queue0, end, Queue) },
	set(QueueGlob, Queue),
	signal(Lock),
	signal(Semaphore).

error(Stream, Msg) -->
	{ Stream = stream(Lock, QueueGlob, Semaphore) },
	wait(Lock),
	get(QueueGlob, Queue0),
	{ queue__put(Queue0, error(Msg), Queue) },
	set(QueueGlob, Queue),
	signal(Lock),
	signal(Semaphore).

get(Stream, Thing) -->
	{ Stream = stream(Lock, QueueGlob, Semaphore) },
	wait(Semaphore),
	wait(Lock),
	get(QueueGlob, Queue0),
	( { queue__get(Queue0, Thing0, Queue) } ->
		{ Thing = Thing0 },
		set(QueueGlob, Queue)
	;
		{ error("stream: queue and semaphore out of sync") }
	),
	signal(Lock).

