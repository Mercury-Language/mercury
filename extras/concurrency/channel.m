%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%---------------------------------------------------------------------------%
%
% Main author:	petdr
% Stability:	low.
%
% A mutvar can only contain a single value, a channel on the otherhand
% provides unbounded buffering.
%
% For example a program could consist of 2 worker threads and one
% logging thread.  The worker threads can place messages into the
% channel, and they will be buffered for processing by the logging
% thread.
%
%---------------------------------------------------------------------------%

:- module channel.

:- interface.

:- import_module io.

:- type channel(T).

	% Initialise a channel.
:- pred channel__init(channel(T)::out, io__state::di, io__state::uo) is det.

	% Put an item at the end of the channel.
:- pred channel__put(channel(T)::in, T::in,
		io__state::di, io__state::uo) is det.

	% Take an item from the start of the channel, block if there is
	% nothing in the channel.
:- pred channel__take(channel(T)::in, T::out,
		io__state::di, io__state::uo) is det.

	% Duplicate a channel.  The new channel sees all (and only) the
	% data written to the channel after the channel__duplicate call.
:- pred channel__duplicate(channel(T)::in, channel(T)::out,
		io__state::di, io__state::uo) is det.

	% Place an item back at the start of the channel.
:- pred channel__untake(channel(T)::in, T::in,
		io__state::di, io__state::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mutvar.

:- type channel(T)
	--->	channel(
			mutvar(stream(T)),	% read end
			mutvar(stream(T))	% write end
		).

:- type stream(T)  == mutvar(item(T)).

:- type item(T)
	--->	item(
			T,		% the current item
			stream(T)	% the rest of the stream
		).

channel__init(channel(Read, Write)) -->
	mutvar__init(Read),
	mutvar__init(Write),
	mutvar__init(Hole),
	mutvar__put(Read, Hole),
	mutvar__put(Write, Hole).

channel__put(channel(_Read, Write), Val) -->
	mutvar__init(NewHole),
	mutvar__take(Write, OldHole),
	mutvar__put(Write, NewHole),
	mutvar__put(OldHole, item(Val, NewHole)).

channel__take(channel(Read, _Write), Val) -->
	mutvar__take(Read, Head),
	mutvar__take(Head, item(Val, NewHead)),
	mutvar__put(Read, NewHead).

channel__duplicate(channel(_Read, Write), channel(NewRead, Write)) -->
	mutvar__init(NewRead),
	mutvar__take(Write, Hole),
	mutvar__put(Write, Hole),
	mutvar__put(NewRead, Hole).

channel__untake(channel(Read, _Write), Val) -->
	mutvar__init(NewHead),
	mutvar__take(Read, Head),
	mutvar__put(NewHead, item(Val, Head)),
	mutvar__put(Read, NewHead).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
