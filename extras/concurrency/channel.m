%---------------------------------------------------------------------------%
% Copyright (C) 2000-2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB
%---------------------------------------------------------------------------%
%
% Main author:	petdr
% Stability:	low.
%
% A mvar can only contain a single value, a channel on the otherhand
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

:- import_module mvar.

:- type channel(T)
	--->	channel(
			mvar(stream(T)),	% read end
			mvar(stream(T))	% write end
		).

:- type stream(T)  == mvar(item(T)).

:- type item(T)
	--->	item(
			T,		% the current item
			stream(T)	% the rest of the stream
		).

channel__init(channel(Read, Write)) -->
	mvar__init(Read),
	mvar__init(Write),
	mvar__init(Hole),
	mvar__put(Read, Hole),
	mvar__put(Write, Hole).

channel__put(channel(_Read, Write), Val) -->
	mvar__init(NewHole),
	mvar__take(Write, OldHole),
	mvar__put(Write, NewHole),
	mvar__put(OldHole, item(Val, NewHole)).

channel__take(channel(Read, _Write), Val) -->
	mvar__take(Read, Head),
	mvar__take(Head, item(Val, NewHead)),
	mvar__put(Read, NewHead).

channel__duplicate(channel(_Read, Write), channel(NewRead, Write)) -->
	mvar__init(NewRead),
	mvar__take(Write, Hole),
	mvar__put(Write, Hole),
	mvar__put(NewRead, Hole).

channel__untake(channel(Read, _Write), Val) -->
	mvar__init(NewHead),
	mvar__take(Read, Head),
	mvar__put(NewHead, item(Val, Head)),
	mvar__put(Read, NewHead).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
