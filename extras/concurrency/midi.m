%---------------------------------------------------------------------------%
% Copyright (C) 2000 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Main author: conway
%
% This module provides routines for concurrently reading and writing MIDI
% streams. MIDI stands for "Musical Instrument Digital Interface" and is a
% hardware and software protocol for electronic instruments to talk to each
% other.
%
%------------------------------------------------------------------------------%

:- module midi.

:- interface.

:- import_module stream.
:- import_module io, list.

	% For information about the meaning
:- type message
	--->	off(channel, note, velocity)	% note off
	;	on(channel, note, velocity)	% note on
	;	kp(channel, note, pressure)	% polyphonic aftertouch
	;	cc(channel, parameter, value)	% controller change
	;	pc(channel, program)		% program change
	;	cp(channel, pressure)		% monophonic aftertouch
	;	pw(channel, pitch_value)	% pitch wheel change
	;	mm(channel, modes)		% mode message
	;	sys(system)			% system message
	;	rt(realtime)			% realtime message
	.

:- type channel	==	int.	% 0 - 15.
:- type note	==	int.	% 0 - 127
:- type velocity ==	int.	% 0 - 127
:- type pressure ==	int.	% 0 - 127
:- type parameter ==	int.	% 0 - 127
:- type value	==	int.	% 0 - 127
:- type program	==	int.	% 0 - 127
:- type pitch_value ==	int.	% 0 - (1 << 14 - 1) biased from 0x2000

:- type modes
	--->	local(onoff)
	;	ano
	;	omni(onoff)
	;	mono(byte)
	;	poly
	.

:- type onoff
	--->	off
	;	on
	.

:- type system
	--->	sysex(list(byte))
	;	pos(int)
	;	sel(byte)
	;	tune
	.

:- type realtime
	--->	clk
	;	start
	;	cont
	;	stop
	;	sense
	;	reset
	.

:- type byte	==	int.

	% Reads from a concurrent stream of bytes and puts its outputs
	% on to a concurrent stream of midi messages.
:- pred read_midi(stream(byte), stream(message), io__state, io__state).
:- mode read_midi(in, in, di, uo) is det.

	% Reads from a concurrent stream of messages, and puts the messages
	% on to a concurrent stream of bytes.
:- pred write_midi(stream(message), stream(byte), io__state, io__state).
:- mode write_midi(in, in, di, uo) is det.

%------------------------------------------------------------------------------%

:- implementation.

:- import_module bool, int, require.

:- type hex
	--->	x0 ; x1 ; x2 ; x3
	;	x4 ; x5 ; x6 ; x7
	;	x8 ; x9 ; xA ; xB
	;	xC ; xD ; xE ; xF
	.

	% This type is used for storing the "running status" used by
	% most MIDI devices, where if the status-byte of two consecutive
	% messages (ignoring any intervening realtime messages) is the
	% same, then it may be omitted in the second message.
:- type status
	--->	none
	;	status(kind, channel).

:- type kind
	--->	one(onebyte)
	;	two(twobyte)
	.

:- type onebyte
	--->	pc
	;	cp
	.

:- type twobyte
	--->	off
	;	on
	;	kp
	;	cc
	;	pw
	.

%------------------------------------------------------------------------------%

	% The midi protocol has two classes of message. The majority of
	% messages including note and controller events fall in the first
	% class. A small number of events - the "realtime" events - can
	% occur in the middle of a normal event. In a sense, they are
	% like out-of-band data.
	%
	% To handle this, the MIDI parser is encoded as a state machine,
	% which remembers any message that is might be half way through
	% parsing when it receives a realtime event.
	%
	% For more info on the MIDI protocol, see a site like
	%	ftp://ftp.ucsd.edu/midi/doc/midi-intro.Z

read_midi(Ins, Outs) -->
	byte0(none, Ins, Outs).

:- pred byte0(status, stream(byte), stream(message),
		io__state, io__state).
:- mode byte0(in, in, in, di, uo) is det.

byte0(Status, Ins, Outs) -->
	get(Ins, Res0),
	(
		{ Res0 = end },
		end(Outs)
	;
		{ Res0 = error(Err) },
		error(Outs, Err)
	;
		{ Res0 = ok(Byte) },
		{ byte2hex(Byte, MSN, LSN) },
		byte0a(MSN, LSN, Status, Ins, Outs)
	).

:- pred byte0a(hex, hex, status, stream(byte), stream(message),
		io__state, io__state).
:- mode byte0a(in, in, in, in, in, di, uo) is det.

byte0a(x0, LSN, Status, Ins, Outs) -->
		{ hex2byte(x0, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte0a(x1, LSN, Status, Ins, Outs) -->
		{ hex2byte(x1, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte0a(x2, LSN, Status, Ins, Outs) -->
		{ hex2byte(x2, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte0a(x3, LSN, Status, Ins, Outs) -->
		{ hex2byte(x3, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte0a(x4, LSN, Status, Ins, Outs) -->
		{ hex2byte(x4, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte0a(x5, LSN, Status, Ins, Outs) -->
		{ hex2byte(x5, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte0a(x6, LSN, Status, Ins, Outs) -->
		{ hex2byte(x6, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte0a(x7, LSN, Status, Ins, Outs) -->
		{ hex2byte(x7, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte0a(x8, LSN, _Status, Ins, Outs) -->
		{ nibble2hex(Chan, LSN) },
		{ Status = status(two(off), Chan) },
		byte1(Status, Ins, Outs).
byte0a(x9, LSN, _Status, Ins, Outs) -->
		{ nibble2hex(Chan, LSN) },
		{ Status = status(two(on), Chan) },
		byte1(Status, Ins, Outs).
byte0a(xA, LSN, _Status, Ins, Outs) -->
		{ nibble2hex(Chan, LSN) },
		{ Status = status(two(kp), Chan) },
		byte1(Status, Ins, Outs).
byte0a(xB, LSN, _Status, Ins, Outs) -->
		{ nibble2hex(Chan, LSN) },
		{ Status = status(two(cc), Chan) },
		byte1(Status, Ins, Outs).
byte0a(xC, LSN, _Status, Ins, Outs) -->
		{ nibble2hex(Chan, LSN) },
		{ Status = status(one(pc), Chan) },
		byte1(Status, Ins, Outs).
byte0a(xD, LSN, _Status, Ins, Outs) -->
		{ nibble2hex(Chan, LSN) },
		{ Status = status(one(cp), Chan) },
		byte1(Status, Ins, Outs).
byte0a(xE, LSN, _Status, Ins, Outs) -->
		{ nibble2hex(Chan, LSN) },
		{ Status = status(two(pw), Chan) },
		byte1(Status, Ins, Outs).
byte0a(xF, x0, Status, Ins, Outs) -->
		sysex0(Status, Ins, Outs).
byte0a(xF, x1, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte0a(xF, x2, Status, Ins, Outs) -->
		pos0(Status, Ins, Outs).
byte0a(xF, x3, Status, Ins, Outs) -->
		sel0(Status, Ins, Outs).
byte0a(xF, x4, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte0a(xF, x5, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte0a(xF, x6, Status, Ins, Outs) -->
		put(Outs, sys(tune)),
		byte0(Status, Ins, Outs).
byte0a(xF, x7, _Status, _Ins, Outs) -->
		error(Outs, "unexpected system byte (byte0)").
byte0a(xF, x8, Status, Ins, Outs) -->
		put(Outs, rt(clk)),
		byte0(Status, Ins, Outs).
byte0a(xF, x9, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte0a(xF, xA, Status, Ins, Outs) -->
		put(Outs, rt(start)),
		byte0(Status, Ins, Outs).
byte0a(xF, xB, Status, Ins, Outs) -->
		put(Outs, rt(cont)),
		byte0(Status, Ins, Outs).
byte0a(xF, xC, Status, Ins, Outs) -->
		put(Outs, rt(stop)),
		byte0(Status, Ins, Outs).
byte0a(xF, xD, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte0a(xF, xE, Status, Ins, Outs) -->
		put(Outs, rt(sense)),
		byte0(Status, Ins, Outs).
byte0a(xF, xF, Status, Ins, Outs) -->
		put(Outs, rt(reset)),
		byte0(Status, Ins, Outs).

:- pred byte1(status, stream(byte), stream(message),
		io__state, io__state).
:- mode byte1(in, in, in, di, uo) is det.

byte1(Status, Ins, Outs) -->
	get(Ins, Res0),
	(
		{ Res0 = end },
		error(Outs, "unexpected end of input")
	;
		{ Res0 = error(Err) },
		error(Outs, Err)
	;
		{ Res0 = ok(Byte) },
		{ byte2hex(Byte, MSN, LSN) },
		byte1a(MSN, LSN, Status, Ins, Outs)
	).

:- pred byte1a(hex, hex, status, stream(byte), stream(message),
		io__state, io__state).
:- mode byte1a(in, in, in, in, in, di, uo) is det.

byte1a(x0, LSN, Status, Ins, Outs) -->
		{ hex2byte(x0, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte1a(x1, LSN, Status, Ins, Outs) -->
		{ hex2byte(x1, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte1a(x2, LSN, Status, Ins, Outs) -->
		{ hex2byte(x2, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte1a(x3, LSN, Status, Ins, Outs) -->
		{ hex2byte(x3, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte1a(x4, LSN, Status, Ins, Outs) -->
		{ hex2byte(x4, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte1a(x5, LSN, Status, Ins, Outs) -->
		{ hex2byte(x5, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte1a(x6, LSN, Status, Ins, Outs) -->
		{ hex2byte(x6, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte1a(x7, LSN, Status, Ins, Outs) -->
		{ hex2byte(x7, LSN, Byte) },
		byte1b(Status, Byte, Ins, Outs).
byte1a(x8, _LSN, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte1a(x9, _LSN, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte1a(xA, _LSN, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte1a(xB, _LSN, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte1a(xC, _LSN, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte1a(xD, _LSN, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte1a(xE, _LSN, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte1a(xF, x0, _Status, _Ins, Outs) -->
		error(Outs, "unexpected system byte").
byte1a(xF, x1, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte1a(xF, x2, _Status, _Ins, Outs) -->
		error(Outs, "unexpected system byte").
byte1a(xF, x3, _Status, _Ins, Outs) -->
		error(Outs, "unexpected system byte").
byte1a(xF, x4, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte1a(xF, x5, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte1a(xF, x6, Status, Ins, Outs) -->
		put(Outs, sys(tune)),
		byte1(Status, Ins, Outs).
byte1a(xF, x7, _Status, _Ins, Outs) -->
		error(Outs, "unexpected system byte").
byte1a(xF, x8, Status, Ins, Outs) -->
		put(Outs, rt(clk)),
		byte1(Status, Ins, Outs).
byte1a(xF, x9, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte1a(xF, xA, Status, Ins, Outs) -->
		put(Outs, rt(start)),
		byte1(Status, Ins, Outs).
byte1a(xF, xB, Status, Ins, Outs) -->
		put(Outs, rt(cont)),
		byte1(Status, Ins, Outs).
byte1a(xF, xC, Status, Ins, Outs) -->
		put(Outs, rt(stop)),
		byte1(Status, Ins, Outs).
byte1a(xF, xD, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte1a(xF, xE, Status, Ins, Outs) -->
		put(Outs, rt(sense)),
		byte1(Status, Ins, Outs).
byte1a(xF, xF, Status, Ins, Outs) -->
		put(Outs, rt(reset)),
		byte1(Status, Ins, Outs).

:- pred byte1b(status, byte, stream(byte), stream(message),
		io__state, io__state).
:- mode byte1b(in, in, in, in, di, uo) is det.

byte1b(none, _Byte, Ins, Outs) -->
	byte0(none, Ins, Outs).
byte1b(status(one(Kind), Chan), Byte, Ins, Outs) -->
	(
		{ Kind = pc },
		{ Msg = pc(Chan, Byte) }
	;
		{ Kind = cp },
		{ Msg = cp(Chan, Byte) }
	),
	put(Outs, Msg),
	byte0(status(one(Kind), Chan), Ins, Outs).
byte1b(status(two(Kind), Chan), Byte1, Ins, Outs) -->
	byte2(status(two(Kind), Chan), Byte1, Ins, Outs).

:- pred byte2(status, byte, stream(byte), stream(message),
		io__state, io__state).
:- mode byte2(in, in, in, in, di, uo) is det.

byte2(Status, Byte1, Ins, Outs) -->
	get(Ins, Res0),
	(
		{ Res0 = end },
		error(Outs, "unexpected end of input")
	;
		{ Res0 = error(Err) },
		error(Outs, Err)
	;
		{ Res0 = ok(Byte2) },
		{ byte2hex(Byte2, MSN2, LSN2) },
		byte2a(MSN2, LSN2, Byte1, Status, Ins, Outs)
	).

:- pred byte2a(hex, hex, byte, status, stream(byte), stream(message),
		io__state, io__state).
:- mode byte2a(in, in, in, in, in, in, di, uo) is det.

byte2a(x0, LSN, Byte1, Status, Ins, Outs) -->
		{ hex2byte(x0, LSN, Byte2) },
		byte2b(Status, Byte1, Byte2, Ins, Outs).
byte2a(x1, LSN, Byte1, Status, Ins, Outs) -->
		{ hex2byte(x1, LSN, Byte2) },
		byte2b(Status, Byte1, Byte2, Ins, Outs).
byte2a(x2, LSN, Byte1, Status, Ins, Outs) -->
		{ hex2byte(x2, LSN, Byte2) },
		byte2b(Status, Byte1, Byte2, Ins, Outs).
byte2a(x3, LSN, Byte1, Status, Ins, Outs) -->
		{ hex2byte(x3, LSN, Byte2) },
		byte2b(Status, Byte1, Byte2, Ins, Outs).
byte2a(x4, LSN, Byte1, Status, Ins, Outs) -->
		{ hex2byte(x4, LSN, Byte2) },
		byte2b(Status, Byte1, Byte2, Ins, Outs).
byte2a(x5, LSN, Byte1, Status, Ins, Outs) -->
		{ hex2byte(x5, LSN, Byte2) },
		byte2b(Status, Byte1, Byte2, Ins, Outs).
byte2a(x6, LSN, Byte1, Status, Ins, Outs) -->
		{ hex2byte(x6, LSN, Byte2) },
		byte2b(Status, Byte1, Byte2, Ins, Outs).
byte2a(x7, LSN, Byte1, Status, Ins, Outs) -->
		{ hex2byte(x7, LSN, Byte2) },
		byte2b(Status, Byte1, Byte2, Ins, Outs).
byte2a(x8, _LSN, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte2a(x9, _LSN, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte2a(xA, _LSN, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte2a(xB, _LSN, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte2a(xC, _LSN, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte2a(xD, _LSN, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte2a(xE, _LSN, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected status byte").
byte2a(xF, x0, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected system byte").
byte2a(xF, x1, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte2a(xF, x2, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected system byte").
byte2a(xF, x3, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected system byte").
byte2a(xF, x4, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte2a(xF, x5, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte2a(xF, x6, Byte1, Status, Ins, Outs) -->
		put(Outs, sys(tune)),
		byte2(Status, Byte1, Ins, Outs).
byte2a(xF, x7, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "unexpected system byte").
byte2a(xF, x8, Byte1, Status, Ins, Outs) -->
		put(Outs, rt(clk)),
		byte2(Status, Byte1, Ins, Outs).
byte2a(xF, x9, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte2a(xF, xA, Byte1, Status, Ins, Outs) -->
		put(Outs, rt(start)),
		byte2(Status, Byte1, Ins, Outs).
byte2a(xF, xB, Byte1, Status, Ins, Outs) -->
		put(Outs, rt(cont)),
		byte2(Status, Byte1, Ins, Outs).
byte2a(xF, xC, Byte1, Status, Ins, Outs) -->
		put(Outs, rt(stop)),
		byte2(Status, Byte1, Ins, Outs).
byte2a(xF, xD, _Byte1, _Status, _Ins, Outs) -->
		error(Outs, "undefined system byte").
byte2a(xF, xE, Byte1, Status, Ins, Outs) -->
		put(Outs, rt(sense)),
		byte2(Status, Byte1, Ins, Outs).
byte2a(xF, xF, Byte1, Status, Ins, Outs) -->
		put(Outs, rt(reset)),
		byte2(Status, Byte1, Ins, Outs).

:- pred byte2b(status, byte, byte, stream(byte), stream(message),
		io__state, io__state).
:- mode byte2b(in, in, in, in, in, di, uo) is det.

byte2b(none, _Byte1, _Byte2, Ins, Outs) -->
	byte0(none, Ins, Outs).
byte2b(status(one(_), _Chan), _Byte1, _Byte2, _Ins, Outs) -->
	error(Outs, "internal error").
byte2b(status(two(Kind), Chan), Byte1, Byte2, Ins, Outs) -->
	(
		{ Kind = off },
		{ Msg = off(Chan, Byte1, Byte2) }
	;
		{ Kind = on },
		{ Msg = on(Chan, Byte1, Byte2) }
	;
		{ Kind = kp },
		{ Msg = kp(Chan, Byte1, Byte2) }
	;
		{ Kind = cc },
		( {
			Byte1 =  122,
			( Byte2 = 0 ->
				OnOrOff = off
			;
				OnOrOff = on
			),
			Msg0 = mm(Chan, local(OnOrOff))
		;
			Byte1 = 123,
			Msg0 = mm(Chan, ano)
		;
			Byte1 = 124,
			Msg0 = mm(Chan, omni(off))
		;
			Byte1 = 125,
			Msg0 = mm(Chan, omni(on))
		;
			Byte1 = 126,
			Msg0 = mm(Chan, mono(Byte2))
		;
			Byte1 = 127,
			Msg0 = mm(Chan, poly)
		} ->
			{ Msg = Msg0 }
		;
			{ Msg = cc(Chan, Byte1, Byte2) }
		)
	;
		{ Kind = pw },
		{ Val = (Byte1 /\ 0x7F) \/ ((Byte2 /\ 0x7F) << 7) },
		{ Msg = pw(Chan, Val) }
	),
	put(Outs, Msg),
	byte0(status(two(Kind), Chan), Ins, Outs).

:- pred sysex0(status, stream(byte), stream(message), io__state, io__state).
:- mode sysex0(in, in, in, di, uo) is det.

sysex0(Status, Ins, Outs) -->
	sysex1([], Status, Ins, Outs).

:- pred sysex1(list(byte), status, stream(byte), stream(message),
		io__state, io__state).
:- mode sysex1(in, in, in, in, di, uo) is det.

sysex1(Bytes0, Status, Ins, Outs) -->
	get(Ins, Res0),
	(
		{ Res0 = end },
		error(Outs, "unexpected end of input")
	;
		{ Res0 = error(Err) },
		error(Outs, Err)
	;
		{ Res0 = ok(Byte) },
		( { Byte >= 0, Byte =< 127 } ->
			sysex1([Byte|Bytes0], Status, Ins, Outs)
		;
			{ reverse(Bytes0, Bytes) },
			put(Outs, sys(sysex(Bytes))),
			( { Byte = 0xF7 } ->
				byte0(Status, Ins, Outs)
			;
				{ byte2hex(Byte, MSN, LSN) },
				byte0a(MSN, LSN, Status, Ins, Outs)
			)
		)
	).

:- pred pos0(status, stream(byte), stream(message), io__state, io__state).
:- mode pos0(in, in, in, di, uo) is det.

pos0(Status, Ins, Outs) -->
	get(Ins, Res0),
	(
		{ Res0 = end },
		error(Outs, "unexpected end of input")
	;
		{ Res0 = error(Err) },
		error(Outs, Err)
	;
		{ Res0 = ok(Byte) },
		pos1(Byte, Status, Ins, Outs)
	).

:- pred pos1(byte, status, stream(byte), stream(message), io__state, io__state).
:- mode pos1(in, in, in, in, di, uo) is det.

pos1(Byte1, Status, Ins, Outs) -->
	get(Ins, Res0),
	(
		{ Res0 = end },
		error(Outs, "unexpected end of input")
	;
		{ Res0 = error(Err) },
		error(Outs, Err)
	;
		{ Res0 = ok(Byte2) },
		{ Val = (Byte1 /\ 0x7F) \/ ((Byte2 /\ 0x7F) << 7) },
		put(Outs, sys(pos(Val))),
		byte0(Status, Ins, Outs)
	).

:- pred sel0(status, stream(byte), stream(message), io__state, io__state).
:- mode sel0(in, in, in, di, uo) is det.

sel0(Status, Ins, Outs) -->
	get(Ins, Res0),
	(
		{ Res0 = end },
		error(Outs, "unexpected end of input")
	;
		{ Res0 = error(Err) },
		error(Outs, Err)
	;
		{ Res0 = ok(Byte) },
		put(Outs, sys(sel(Byte))),
		byte0(Status, Ins, Outs)
	).

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

write_midi(Ins, Outs) -->
	write_midi(none, Ins, Outs).

:- pred write_midi(status, stream(message), stream(byte), io__state, io__state).
:- mode write_midi(in, in, in, di, uo) is det.

write_midi(Status, Ins, Outs) -->
	get(Ins, Res0),
	(
		{ Res0 = end },
		end(Outs)
	;
		{ Res0 = error(Msg) },
		error(Outs, Msg)
	;
		{ Res0 = ok(Msg) },
		write_midi(Msg, Status, Ins, Outs)
	).

:- pred write_midi(message, status, stream(message), stream(byte),
		io__state, io__state).
:- mode write_midi(in, in, in, in, di, uo) is det.

write_midi(off(Chan, Note, Vel), Status0, Ins, Outs) -->
	{ Status1 = status(two(off), Chan) },
	write_two(Status0, Status1, Note, Vel, Ins, Outs).
write_midi(on(Chan, Note, Vel), Status0, Ins, Outs) -->
	{ Status1 = status(two(on), Chan) },
	write_two(Status0, Status1, Note, Vel, Ins, Outs).
write_midi(kp(Chan, Note, Press), Status0, Ins, Outs) -->
	{ Status1 = status(two(kp), Chan) },
	write_two(Status0, Status1, Note, Press, Ins, Outs).
write_midi(cc(Chan, Ctrl, Val), Status0, Ins, Outs) -->
	{ Status1 = status(two(cc), Chan) },
	write_two(Status0, Status1, Ctrl, Val, Ins, Outs).
write_midi(pc(Chan, Prog), Status0, Ins, Outs) -->
	{ Status1 = status(one(pc), Chan) },
	write_one(Status0, Status1, Prog, Ins, Outs).
write_midi(cp(Chan, Press), Status0, Ins, Outs) -->
	{ Status1 = status(one(cp), Chan) },
	write_one(Status0, Status1, Press, Ins, Outs).
write_midi(pw(Chan, Val), Status0, Ins, Outs) -->
	{ Status1 = status(two(pw), Chan) },
	{ Byte1 = Val /\ 0x7F },
	{ Byte2 = (Val >> 7) /\ 0x7F },
	write_two(Status0, Status1, Byte1, Byte2, Ins, Outs).
write_midi(mm(Chan, Mode), Status0, Ins, Outs) -->
	{ Status1 = status(two(cc), Chan) },
	(
		{ Mode = local(off) },
		{ Byte1 = 122, Byte2 = 0 }
	;
		{ Mode = local(on) },
		{ Byte1 = 122, Byte2 = 127 }
	;
		{ Mode = ano },
		{ Byte1 = 123, Byte2 = 0 }
	;
		{ Mode = omni(off) },
		{ Byte1 = 124, Byte2 = 0 }
	;
		{ Mode = omni(on) },
		{ Byte1 = 125, Byte2 = 0 }
	;
		{ Mode = mono(N) },
		{ Byte1 = 126, Byte2 = N /\ 0x7F }
	;
		{ Mode = poly },
		{ Byte1 = 127, Byte2 = 0 }
	),
	write_two(Status0, Status1, Byte1, Byte2, Ins, Outs).
write_midi(sys(sysex(Bytes)), Status, Ins, Outs) -->
	put(Outs, 0xF0),
	foldl((pred(Byte::in, di, uo) is det -->
		( { Byte >= 0, Byte =< 127 } ->
			put(Outs, Byte)
		;
			error(Outs, "sysex data byte out of range")
		)
	), Bytes),
	put(Outs, 0xF7),
	write_midi(Status, Ins, Outs).
write_midi(sys(pos(Pos)), Status, Ins, Outs) -->
	put(Outs, 0xF2),
	{ Byte1 = Pos /\ 0x7F },
	{ Byte2 = (Pos >> 7) /\ 0x7F },
	put(Outs, Byte1),
	put(Outs, Byte2),
	write_midi(Status, Ins, Outs).
write_midi(sys(sel(Sel)), Status, Ins, Outs) -->
	put(Outs, 0xF3),
	put(Outs, Sel),
	write_midi(Status, Ins, Outs).
write_midi(sys(tune), Status, Ins, Outs) -->
	put(Outs, 0xF6),
	write_midi(Status, Ins, Outs).
write_midi(rt(clk), Status, Ins, Outs) -->
	put(Outs, 0xF8),
	write_midi(Status, Ins, Outs).
write_midi(rt(start), Status, Ins, Outs) -->
	put(Outs, 0xFA),
	write_midi(Status, Ins, Outs).
write_midi(rt(cont), Status, Ins, Outs) -->
	put(Outs, 0xFB),
	write_midi(Status, Ins, Outs).
write_midi(rt(stop), Status, Ins, Outs) -->
	put(Outs, 0xFC),
	write_midi(Status, Ins, Outs).
write_midi(rt(sense), Status, Ins, Outs) -->
	put(Outs, 0xFE),
	write_midi(Status, Ins, Outs).
write_midi(rt(reset), Status, Ins, Outs) -->
	put(Outs, 0xFF),
	write_midi(Status, Ins, Outs).

:- pred write_one(status, status, byte, stream(message), stream(byte),
		io__state, io__state).
:- mode write_one(in, in, in, in, in, di, uo) is det.

write_one(Status0, Status1, Byte1, Ins, Outs) -->
	( { Status0 = Status1 } ->
		{ Status = Status0 }
	;
		{ Status = Status1 },
		( { status(Status, Byte) } ->
			put(Outs, Byte)
		;
			error(Outs, "invalid channel")
		)
	),
	( { Byte1 >= 0, Byte1 =< 127 } ->
		put(Outs, Byte1)
	;
		error(Outs, "invalid data byte")
	),
	write_midi(Status, Ins, Outs).

:- pred write_two(status, status, byte, byte, stream(message), stream(byte),
		io__state, io__state).
:- mode write_two(in, in, in, in, in, in, di, uo) is det.

write_two(Status0, Status1, Byte1, Byte2, Ins, Outs) -->
	( { Status0 = Status1 } ->
		{ Status = Status0 }
	;
		{ Status = Status1 },
		( { status(Status, Byte) } ->
			put(Outs, Byte)
		;
			error(Outs, "invalid channel")
		)
	),
	( { Byte1 >= 0, Byte1 =< 127 } ->
		put(Outs, Byte1)
	;
		error(Outs, "invalid data byte")
	),
	( { Byte2 >= 0, Byte2 =< 127 } ->
		put(Outs, Byte2)
	;
		error(Outs, "invalid data byte")
	),
	write_midi(Status, Ins, Outs).

:- pred status(status, byte).
:- mode status(in, out) is semidet.

status(none, _) :-
	error("status: no status").
status(status(Kind, Chan), Byte) :-
	Chan >= 0, Chan =< 15,
	(
	  Kind = two(off), Nib = 0x80
	; Kind = two(on), Nib = 0x90
	; Kind = two(kp), Nib = 0xA0
	; Kind = two(cc), Nib = 0xB0
	; Kind = one(pc), Nib = 0xC0
	; Kind = one(cp), Nib = 0xD0
	; Kind = two(pw), Nib = 0xE0
	),
	Byte = Nib \/ Chan.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- pred byte2hex(int, hex, hex).
:- mode byte2hex(in, out, out) is det.

byte2hex(Byte, MSN, LSN) :-
	(
		nibble2hex(Byte /\ 0xF, LSN0),
		nibble2hex((Byte >> 4) /\ 0xF, MSN0)
	->
		LSN = LSN0,
		MSN = MSN0
	;
		error("byte2hex: conversion failed!")
	).

:- pred hex2byte(hex, hex, int).
:- mode hex2byte(in, in, out) is det.

hex2byte(MSN, LSN, Byte) :-
	nibble2hex(A, MSN),
	nibble2hex(B, LSN),
	Byte = B \/ (A << 4).

:- pred nibble2hex(int, hex).
:- mode nibble2hex(in, out) is semidet.
:- mode nibble2hex(out, in) is det.

nibble2hex(0x0,	x0).
nibble2hex(0x1,	x1).
nibble2hex(0x2,	x2).
nibble2hex(0x3,	x3).
nibble2hex(0x4,	x4).
nibble2hex(0x5,	x5).
nibble2hex(0x6,	x6).
nibble2hex(0x7,	x7).
nibble2hex(0x8,	x8).
nibble2hex(0x9,	x9).
nibble2hex(0xA,	xA).
nibble2hex(0xB,	xB).
nibble2hex(0xC,	xC).
nibble2hex(0xD,	xD).
nibble2hex(0xE,	xE).
nibble2hex(0xF,	xF).

