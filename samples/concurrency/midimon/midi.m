%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2000, 2006, 2010 The University of Melbourne.
% Copyright (C) 2016, 2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: midi.m.
% Main author: conway.
%
% This module provides routines for concurrently reading and writing MIDI
% streams. MIDI stands for "Musical Instrument Digital Interface" and is a
% hardware and software protocol for electronic instruments to talk to each
% other.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module midi.
:- interface.

:- import_module concurrent_stream.

:- import_module io.
:- import_module list.

%-----------------------------------------------------------------------------%

    % For information about the meaning
    %
:- type message
    --->    off(channel, note, velocity)    % note off
    ;       on(channel, note, velocity)     % note on
    ;       kp(channel, note, pressure)     % polyphonic aftertouch
    ;       cc(channel, parameter, value)   % controller change
    ;       pc(channel, program)            % program change
    ;       cp(channel, pressure)           % monophonic aftertouch
    ;       pw(channel, pitch_value)        % pitch wheel change
    ;       mm(channel, modes)              % mode message
    ;       sys(system)                     % system message
    ;       rt(realtime).                   % realtime message

:- type channel ==  int.        % 0 - 15.
:- type note    ==  int.        % 0 - 127
:- type velocity == int.        % 0 - 127
:- type pressure == int.        % 0 - 127
:- type parameter == int.       % 0 - 127
:- type value   ==  int.        % 0 - 127
:- type program ==  int.        % 0 - 127
:- type pitch_value == int.     % 0 - (1 << 14 - 1) biased from 0x2000

:- type modes
    --->    local(onoff)
    ;       ano
    ;       omni(onoff)
    ;       mono(byte)
    ;       poly.

:- type onoff
    --->    off
    ;       on.

:- type system
    --->    sysex(list(byte))
    ;       pos(int)
    ;       sel(byte)
    ;       tune.

:- type realtime
    --->    clk
    ;       start
    ;       cont
    ;       stop
    ;       sense
    ;       reset.

:- type byte == int.

    % Reads from a concurrent stream of bytes and puts its outputs
    % on to a concurrent stream of midi messages.
    %
:- pred read_midi(concurrent_stream(byte)::in, concurrent_stream(message)::in,
    io::di, io::uo) is det.

    % Reads from a concurrent stream of messages, and puts the messages
    % on to a concurrent stream of bytes.
    %
:- pred write_midi(concurrent_stream(message)::in, concurrent_stream(byte)::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module require.

%-----------------------------------------------------------------------------%

:- type hex
    --->    x0
    ;       x1
    ;       x2
    ;       x3
    ;       x4
    ;       x5
    ;       x6
    ;       x7
    ;       x8
    ;       x9
    ;       xA
    ;       xB
    ;       xC
    ;       xD
    ;       xE
    ;       xF.

    % This type is used for storing the "running status" used by
    % most MIDI devices, where if the status-byte of two consecutive
    % messages (ignoring any intervening realtime messages) is the
    % same, then it may be omitted in the second message.
    %
:- type status
    --->    none
    ;       status(kind, channel).

:- type kind
    --->    one(onebyte)
    ;       two(twobyte).

:- type onebyte
    --->    pc
    ;       cp.

:- type twobyte
    --->    off
    ;       on
    ;       kp
    ;       cc
    ;       pw.

%-----------------------------------------------------------------------------%

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
    %   <ftp://ftp.ucsd.edu/midi/doc/midi-intro.Z>

read_midi(Ins, Outs, !IO) :-
    byte0(none, Ins, Outs, !IO).

:- pred byte0(status::in, concurrent_stream(byte)::in,
    concurrent_stream(message)::in, io::di, io::uo) is det.

byte0(Status, Ins, Outs, !IO) :-
    get(Ins, Res0, !IO),
    (
        Res0 = end,
        end(Outs, !IO)
    ;
        Res0 = error(Err),
        error(Outs, Err, !IO)
    ;
        Res0 = ok(Byte),
        byte2hex(Byte, MSN, LSN),
        byte0a(MSN, LSN, Status, Ins, Outs, !IO)
    ).

:- pred byte0a(hex::in, hex::in, status::in,
    concurrent_stream(byte)::in, concurrent_stream(message)::in,
    io::di, io::uo) is det.

byte0a(x0, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x0, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte0a(x1, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x1, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte0a(x2, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x2, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte0a(x3, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x3, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte0a(x4, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x4, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte0a(x5, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x5, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte0a(x6, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x6, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte0a(x7, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x7, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte0a(x8, LSN, _Status, Ins, Outs, !IO) :-
    nibble2hex(Chan, LSN),
    Status = status(two(off), Chan),
    byte1(Status, Ins, Outs, !IO).
byte0a(x9, LSN, _Status, Ins, Outs, !IO) :-
    nibble2hex(Chan, LSN),
    Status = status(two(on), Chan),
    byte1(Status, Ins, Outs, !IO).
byte0a(xA, LSN, _Status, Ins, Outs, !IO) :-
    nibble2hex(Chan, LSN),
    Status = status(two(kp), Chan),
    byte1(Status, Ins, Outs, !IO).
byte0a(xB, LSN, _Status, Ins, Outs, !IO) :-
    nibble2hex(Chan, LSN),
    Status = status(two(cc), Chan),
    byte1(Status, Ins, Outs, !IO).
byte0a(xC, LSN, _Status, Ins, Outs, !IO) :-
    nibble2hex(Chan, LSN),
    Status = status(one(pc), Chan),
    byte1(Status, Ins, Outs, !IO).
byte0a(xD, LSN, _Status, Ins, Outs, !IO) :-
    nibble2hex(Chan, LSN),
    Status = status(one(cp), Chan),
    byte1(Status, Ins, Outs, !IO).
byte0a(xE, LSN, _Status, Ins, Outs, !IO) :-
    nibble2hex(Chan, LSN),
    Status = status(two(pw), Chan),
    byte1(Status, Ins, Outs, !IO).
byte0a(xF, x0, Status, Ins, Outs, !IO) :-
    sysex0(Status, Ins, Outs, !IO).
byte0a(xF, x1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte0a(xF, x2, Status, Ins, Outs, !IO)  :-
    pos0(Status, Ins, Outs, !IO).
byte0a(xF, x3, Status, Ins, Outs, !IO) :-
    sel0(Status, Ins, Outs, !IO).
byte0a(xF, x4, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte0a(xF, x5, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte0a(xF, x6, Status, Ins, Outs, !IO) :-
    put(Outs, sys(tune), !IO),
    byte0(Status, Ins, Outs, !IO).
byte0a(xF, x7, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected system byte (byte0)", !IO).
byte0a(xF, x8, Status, Ins, Outs, !IO) :-
    put(Outs, rt(clk), !IO),
    byte0(Status, Ins, Outs, !IO).
byte0a(xF, x9, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte0a(xF, xA, Status, Ins, Outs, !IO) :-
    put(Outs, rt(start), !IO),
    byte0(Status, Ins, Outs, !IO).
byte0a(xF, xB, Status, Ins, Outs, !IO) :-
    put(Outs, rt(cont), !IO),
    byte0(Status, Ins, Outs, !IO).
byte0a(xF, xC, Status, Ins, Outs, !IO) :-
    put(Outs, rt(stop), !IO),
    byte0(Status, Ins, Outs, !IO).
byte0a(xF, xD, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte0a(xF, xE, Status, Ins, Outs, !IO) :-
    put(Outs, rt(sense), !IO),
    byte0(Status, Ins, Outs, !IO).
byte0a(xF, xF, Status, Ins, Outs, !IO) :-
    put(Outs, rt(reset), !IO),
    byte0(Status, Ins, Outs, !IO).

:- pred byte1(status::in, concurrent_stream(byte)::in,
    concurrent_stream(message)::in, io::di, io::uo) is det.

byte1(Status, Ins, Outs, !IO) :-
    get(Ins, Res0, !IO),
    (
        Res0 = end,
        error(Outs, "unexpected end of input", !IO)
    ;
        Res0 = error(Err),
        error(Outs, Err, !IO)
    ;
        Res0 = ok(Byte),
        byte2hex(Byte, MSN, LSN),
        byte1a(MSN, LSN, Status, Ins, Outs, !IO)
    ).

:- pred byte1a(hex::in, hex::in, status::in,
    concurrent_stream(byte)::in, concurrent_stream(message)::in,
    io::di, io::uo) is det.

byte1a(x0, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x0, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte1a(x1, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x1, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte1a(x2, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x2, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte1a(x3, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x3, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte1a(x4, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x4, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte1a(x5, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x5, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte1a(x6, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x6, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte1a(x7, LSN, Status, Ins, Outs, !IO) :-
    hex2byte(x7, LSN, Byte),
    byte1b(Status, Byte, Ins, Outs, !IO).
byte1a(x8, _LSN, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte1a(x9, _LSN, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte1a(xA, _LSN, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte1a(xB, _LSN, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte1a(xC, _LSN, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte1a(xD, _LSN, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte1a(xE, _LSN, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte1a(xF, x0, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected system byte", !IO).
byte1a(xF, x1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte1a(xF, x2, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected system byte", !IO).
byte1a(xF, x3, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected system byte", !IO).
byte1a(xF, x4, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte1a(xF, x5, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte1a(xF, x6, Status, Ins, Outs, !IO) :-
    put(Outs, sys(tune), !IO),
    byte1(Status, Ins, Outs, !IO).
byte1a(xF, x7, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected system byte", !IO).
byte1a(xF, x8, Status, Ins, Outs, !IO) :-
    put(Outs, rt(clk), !IO),
    byte1(Status, Ins, Outs, !IO).
byte1a(xF, x9, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte1a(xF, xA, Status, Ins, Outs, !IO) :-
    put(Outs, rt(start), !IO),
    byte1(Status, Ins, Outs, !IO).
byte1a(xF, xB, Status, Ins, Outs, !IO) :-
    put(Outs, rt(cont), !IO),
    byte1(Status, Ins, Outs, !IO).
byte1a(xF, xC, Status, Ins, Outs, !IO) :-
    put(Outs, rt(stop), !IO),
    byte1(Status, Ins, Outs, !IO).
byte1a(xF, xD, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte1a(xF, xE, Status, Ins, Outs, !IO) :-
    put(Outs, rt(sense), !IO),
    byte1(Status, Ins, Outs, !IO).
byte1a(xF, xF, Status, Ins, Outs, !IO) :-
    put(Outs, rt(reset), !IO),
    byte1(Status, Ins, Outs, !IO).

:- pred byte1b(status::in, byte::in, concurrent_stream(byte)::in,
    concurrent_stream(message)::in, io::di, io::uo) is det.

byte1b(none, _Byte, Ins, Outs, !IO) :-
    byte0(none, Ins, Outs, !IO).
byte1b(status(one(Kind), Chan), Byte, Ins, Outs, !IO) :-
    (
        Kind = pc,
        Msg = pc(Chan, Byte)
    ;
        Kind = cp,
        Msg = cp(Chan, Byte)
    ),
    put(Outs, Msg, !IO),
    byte0(status(one(Kind), Chan), Ins, Outs, !IO).
byte1b(status(two(Kind), Chan), Byte1, Ins, Outs, !IO) :-
    byte2(status(two(Kind), Chan), Byte1, Ins, Outs, !IO).

:- pred byte2(status::in, byte::in, concurrent_stream(byte)::in,
    concurrent_stream(message)::in, io::di, io::uo) is det.

byte2(Status, Byte1, Ins, Outs, !IO) :-
    get(Ins, Res0, !IO),
    (
        Res0 = end,
        error(Outs, "unexpected end of input", !IO)
    ;
        Res0 = error(Err),
        error(Outs, Err, !IO)
    ;
        Res0 = ok(Byte2),
        byte2hex(Byte2, MSN2, LSN2),
        byte2a(MSN2, LSN2, Byte1, Status, Ins, Outs, !IO)
    ).

:- pred byte2a(hex::in, hex::in, byte::in, status::in,
    concurrent_stream(byte)::in, concurrent_stream(message)::in,
    io::di, io::uo) is det.

byte2a(x0, LSN, Byte1, Status, Ins, Outs, !IO) :-
    hex2byte(x0, LSN, Byte2),
    byte2b(Status, Byte1, Byte2, Ins, Outs, !IO).
byte2a(x1, LSN, Byte1, Status, Ins, Outs, !IO) :-
    hex2byte(x1, LSN, Byte2),
    byte2b(Status, Byte1, Byte2, Ins, Outs, !IO).
byte2a(x2, LSN, Byte1, Status, Ins, Outs, !IO) :-
    hex2byte(x2, LSN, Byte2),
    byte2b(Status, Byte1, Byte2, Ins, Outs, !IO).
byte2a(x3, LSN, Byte1, Status, Ins, Outs, !IO) :-
    hex2byte(x3, LSN, Byte2),
    byte2b(Status, Byte1, Byte2, Ins, Outs, !IO).
byte2a(x4, LSN, Byte1, Status, Ins, Outs, !IO) :-
    hex2byte(x4, LSN, Byte2),
    byte2b(Status, Byte1, Byte2, Ins, Outs, !IO).
byte2a(x5, LSN, Byte1, Status, Ins, Outs, !IO) :-
    hex2byte(x5, LSN, Byte2),
    byte2b(Status, Byte1, Byte2, Ins, Outs, !IO).
byte2a(x6, LSN, Byte1, Status, Ins, Outs, !IO) :-
    hex2byte(x6, LSN, Byte2),
    byte2b(Status, Byte1, Byte2, Ins, Outs, !IO).
byte2a(x7, LSN, Byte1, Status, Ins, Outs, !IO) :-
    hex2byte(x7, LSN, Byte2),
    byte2b(Status, Byte1, Byte2, Ins, Outs, !IO).
byte2a(x8, _LSN, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte2a(x9, _LSN, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte2a(xA, _LSN, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte2a(xB, _LSN, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte2a(xC, _LSN, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte2a(xD, _LSN, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte2a(xE, _LSN, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected status byte", !IO).
byte2a(xF, x0, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected system byte", !IO).
byte2a(xF, x1, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte2a(xF, x2, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected system byte", !IO).
byte2a(xF, x3, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected system byte", !IO).
byte2a(xF, x4, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte2a(xF, x5, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte2a(xF, x6, Byte1, Status, Ins, Outs, !IO) :-
    put(Outs, sys(tune), !IO),
    byte2(Status, Byte1, Ins, Outs, !IO).
byte2a(xF, x7, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "unexpected system byte", !IO).
byte2a(xF, x8, Byte1, Status, Ins, Outs, !IO) :-
    put(Outs, rt(clk), !IO),
    byte2(Status, Byte1, Ins, Outs, !IO).
byte2a(xF, x9, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte2a(xF, xA, Byte1, Status, Ins, Outs, !IO) :-
    put(Outs, rt(start), !IO),
    byte2(Status, Byte1, Ins, Outs, !IO).
byte2a(xF, xB, Byte1, Status, Ins, Outs, !IO) :-
    put(Outs, rt(cont), !IO),
    byte2(Status, Byte1, Ins, Outs, !IO).
byte2a(xF, xC, Byte1, Status, Ins, Outs, !IO) :-
    put(Outs, rt(stop), !IO),
    byte2(Status, Byte1, Ins, Outs, !IO).
byte2a(xF, xD, _Byte1, _Status, _Ins, Outs, !IO) :-
    error(Outs, "undefined system byte", !IO).
byte2a(xF, xE, Byte1, Status, Ins, Outs, !IO) :-
    put(Outs, rt(sense), !IO),
    byte2(Status, Byte1, Ins, Outs, !IO).
byte2a(xF, xF, Byte1, Status, Ins, Outs, !IO) :-
    put(Outs, rt(reset), !IO),
    byte2(Status, Byte1, Ins, Outs, !IO).

:- pred byte2b(status::in, byte::in, byte::in,
    concurrent_stream(byte)::in, concurrent_stream(message)::in,
    io::di, io::uo) is det.

byte2b(none, _Byte1, _Byte2, Ins, Outs, !IO) :-
    byte0(none, Ins, Outs, !IO).
byte2b(status(one(_), _Chan), _Byte1, _Byte2, _Ins, Outs, !IO) :-
    error(Outs, "internal error", !IO).
byte2b(status(two(Kind), Chan), Byte1, Byte2, Ins, Outs, !IO) :-
    (
        Kind = off,
        Msg = off(Chan, Byte1, Byte2)
    ;
        Kind = on,
        Msg = on(Chan, Byte1, Byte2)
    ;
        Kind = kp,
        Msg = kp(Chan, Byte1, Byte2)
    ;
        Kind = cc,
        ( if
            (
                Byte1 =  122,
                OnOrOff = ( Byte2 = 0 -> off ; on ),
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
            )
        then
            Msg = Msg0
        else
            Msg = cc(Chan, Byte1, Byte2)
        )
    ;
        Kind = pw,
        Val = (Byte1 /\ 0x7F) \/ ((Byte2 /\ 0x7F) << 7),
        Msg = pw(Chan, Val)
    ),
    put(Outs, Msg, !IO),
    byte0(status(two(Kind), Chan), Ins, Outs, !IO).

:- pred sysex0(status::in, concurrent_stream(byte)::in,
    concurrent_stream(message)::in, io::di, io::uo) is det.

sysex0(Status, Ins, Outs, !IO) :-
    sysex1([], Status, Ins, Outs, !IO).

:- pred sysex1(list(byte)::in, status::in, concurrent_stream(byte)::in,
    concurrent_stream(message)::in, io::di, io::uo) is det.

sysex1(Bytes0, Status, Ins, Outs, !IO) :-
    get(Ins, Res0, !IO),
    (
        Res0 = end,
        error(Outs, "unexpected end of input", !IO)
    ;
        Res0 = error(Err),
        error(Outs, Err, !IO)
    ;
        Res0 = ok(Byte),
        ( if Byte >= 0, Byte =< 127 then
            sysex1([Byte|Bytes0], Status, Ins, Outs, !IO)
        else
            list.reverse(Bytes0, Bytes),
            put(Outs, sys(sysex(Bytes)), !IO),
            ( if Byte = 0xF7 then
                byte0(Status, Ins, Outs, !IO)
            else
                byte2hex(Byte, MSN, LSN),
                byte0a(MSN, LSN, Status, Ins, Outs, !IO)
            )
        )
    ).

:- pred pos0(status::in, concurrent_stream(byte)::in,
    concurrent_stream(message)::in, io::di, io::uo) is det.

pos0(Status, Ins, Outs, !IO) :-
    get(Ins, Res0, !IO),
    (
        Res0 = end,
        error(Outs, "unexpected end of input", !IO)
    ;
        Res0 = error(Err),
        error(Outs, Err, !IO)
    ;
        Res0 = ok(Byte),
        pos1(Byte, Status, Ins, Outs, !IO)
    ).

:- pred pos1(byte::in, status::in, concurrent_stream(byte)::in,
    concurrent_stream(message)::in, io::di, io::uo) is det.

pos1(Byte1, Status, Ins, Outs, !IO) :-
    get(Ins, Res0, !IO),
    (
        Res0 = end,
        error(Outs, "unexpected end of input", !IO)
    ;
        Res0 = error(Err),
        error(Outs, Err, !IO)
    ;
        Res0 = ok(Byte2),
        Val = (Byte1 /\ 0x7F) \/ ((Byte2 /\ 0x7F) << 7),
        put(Outs, sys(pos(Val)), !IO),
        byte0(Status, Ins, Outs, !IO)
    ).

:- pred sel0(status::in, concurrent_stream(byte)::in,
    concurrent_stream(message)::in, io::di, io::uo) is det.

sel0(Status, Ins, Outs, !IO) :-
    get(Ins, Res0, !IO),
    (
        Res0 = end,
        error(Outs, "unexpected end of input", !IO)
    ;
        Res0 = error(Err),
        error(Outs, Err, !IO)
    ;
        Res0 = ok(Byte),
        put(Outs, sys(sel(Byte)), !IO),
        byte0(Status, Ins, Outs, !IO)
    ).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

write_midi(Ins, Outs, !IO) :-
    write_midi(none, Ins, Outs, !IO).

:- pred write_midi(status::in, concurrent_stream(message)::in,
    concurrent_stream(byte)::in, io::di, io::uo) is det.

write_midi(Status, Ins, Outs, !IO) :-
    get(Ins, Res0, !IO),
    (
        Res0 = end,
        end(Outs, !IO)
    ;
        Res0 = error(Msg),
        error(Outs, Msg, !IO)
    ;
        Res0 = ok(Msg),
        write_midi(Msg, Status, Ins, Outs, !IO)
    ).

:- pred write_midi(message::in, status::in, concurrent_stream(message)::in,
    concurrent_stream(byte)::in, io::di, io::uo) is det.

write_midi(off(Chan, Note, Vel), Status0, Ins, Outs, !IO) :-
    Status1 = status(two(off), Chan),
    write_two(Status0, Status1, Note, Vel, Ins, Outs, !IO).
write_midi(on(Chan, Note, Vel), Status0, Ins, Outs, !IO) :-
    Status1 = status(two(on), Chan),
    write_two(Status0, Status1, Note, Vel, Ins, Outs, !IO).
write_midi(kp(Chan, Note, Press), Status0, Ins, Outs, !IO) :-
    Status1 = status(two(kp), Chan),
    write_two(Status0, Status1, Note, Press, Ins, Outs, !IO).
write_midi(cc(Chan, Ctrl, Val), Status0, Ins, Outs, !IO) :-
    Status1 = status(two(cc), Chan),
    write_two(Status0, Status1, Ctrl, Val, Ins, Outs, !IO).
write_midi(pc(Chan, Prog), Status0, Ins, Outs, !IO) :-
    Status1 = status(one(pc), Chan),
    write_one(Status0, Status1, Prog, Ins, Outs, !IO).
write_midi(cp(Chan, Press), Status0, Ins, Outs, !IO) :-
    Status1 = status(one(cp), Chan),
    write_one(Status0, Status1, Press, Ins, Outs, !IO).
write_midi(pw(Chan, Val), Status0, Ins, Outs, !IO) :-
    Status1 = status(two(pw), Chan),
    Byte1 = Val /\ 0x7F,
    Byte2 = (Val >> 7) /\ 0x7F,
    write_two(Status0, Status1, Byte1, Byte2, Ins, Outs, !IO).
write_midi(mm(Chan, Mode), Status0, Ins, Outs, !IO) :-
    Status1 = status(two(cc), Chan),
    (
        Mode = local(off),
        Byte1 = 122, Byte2 = 0
    ;
        Mode = local(on),
        Byte1 = 122, Byte2 = 127
    ;
        Mode = ano,
        Byte1 = 123, Byte2 = 0
    ;
        Mode = omni(off),
        Byte1 = 124, Byte2 = 0
    ;
        Mode = omni(on),
        Byte1 = 125, Byte2 = 0
    ;
        Mode = mono(N),
        Byte1 = 126, Byte2 = N /\ 0x7F
    ;
        Mode = poly,
        Byte1 = 127, Byte2 = 0
    ),
    write_two(Status0, Status1, Byte1, Byte2, Ins, Outs, !IO).
write_midi(sys(sysex(Bytes)), Status, Ins, Outs, !IO) :-
    put(Outs, 0xF0, !IO),
    PutByte = (pred(Byte::in, !.IO::di, !:IO::uo) is det :-
        ( Byte >= 0, Byte =< 127 ->
            put(Outs, Byte, !IO)
        ;
            error(Outs, "sysex data byte out of range", !IO)
        )
    ),
    list.foldl(PutByte, Bytes, !IO),
    put(Outs, 0xF7, !IO),
    write_midi(Status, Ins, Outs, !IO).
write_midi(sys(pos(Pos)), Status, Ins, Outs, !IO) :-
    put(Outs, 0xF2, !IO),
    Byte1 = Pos /\ 0x7F,
    Byte2 = (Pos >> 7) /\ 0x7F,
    put(Outs, Byte1, !IO),
    put(Outs, Byte2, !IO),
    write_midi(Status, Ins, Outs, !IO).
write_midi(sys(sel(Sel)), Status, Ins, Outs, !IO) :-
    put(Outs, 0xF3, !IO),
    put(Outs, Sel, !IO),
    write_midi(Status, Ins, Outs, !IO).
write_midi(sys(tune), Status, Ins, Outs, !IO) :-
    put(Outs, 0xF6, !IO),
    write_midi(Status, Ins, Outs, !IO).
write_midi(rt(clk), Status, Ins, Outs, !IO) :-
    put(Outs, 0xF8, !IO),
    write_midi(Status, Ins, Outs, !IO).
write_midi(rt(start), Status, Ins, Outs, !IO) :-
    put(Outs, 0xFA, !IO),
    write_midi(Status, Ins, Outs, !IO).
write_midi(rt(cont), Status, Ins, Outs, !IO) :-
    put(Outs, 0xFB, !IO),
    write_midi(Status, Ins, Outs, !IO).
write_midi(rt(stop), Status, Ins, Outs, !IO) :-
    put(Outs, 0xFC, !IO),
    write_midi(Status, Ins, Outs, !IO).
write_midi(rt(sense), Status, Ins, Outs, !IO) :-
    put(Outs, 0xFE, !IO),
    write_midi(Status, Ins, Outs, !IO).
write_midi(rt(reset), Status, Ins, Outs, !IO) :-
    put(Outs, 0xFF, !IO),
    write_midi(Status, Ins, Outs, !IO).

:- pred write_one(status::in, status::in, byte::in,
    concurrent_stream(message)::in, concurrent_stream(byte)::in,
    io::di, io::uo) is det.

write_one(Status0, Status1, Byte1, Ins, Outs, !IO) :-
    ( if Status0 = Status1 then
        Status = Status0
    else
        Status = Status1,
        ( if status(Status, Byte) then
            put(Outs, Byte, !IO)
        else
            error(Outs, "invalid channel", !IO)
        )
    ),
    ( if Byte1 >= 0, Byte1 =< 127 then
        put(Outs, Byte1, !IO)
    else
        error(Outs, "invalid data byte", !IO)
    ),
    write_midi(Status, Ins, Outs, !IO).

:- pred write_two(status::in, status::in, byte::in, byte::in,
    concurrent_stream(message)::in,
    concurrent_stream(byte)::in, io::di, io::uo) is det.

write_two(Status0, Status1, Byte1, Byte2, Ins, Outs, !IO) :-
    ( if Status0 = Status1 then
        Status = Status0
    else
        Status = Status1,
        ( if status(Status, Byte) then
            put(Outs, Byte, !IO)
        else
            error(Outs, "invalid channel", !IO)
        )
    ),
    ( if Byte1 >= 0, Byte1 =< 127 then
        put(Outs, Byte1, !IO)
    else
        error(Outs, "invalid data byte", !IO)
    ),
    ( if Byte2 >= 0, Byte2 =< 127 then
        put(Outs, Byte2, !IO)
    else
        error(Outs, "invalid data byte", !IO)
    ),
    write_midi(Status, Ins, Outs, !IO).

:- pred status(status::in, byte::out) is semidet.

status(none, _) :-
    error("status: no status").
status(status(Kind, Chan), Byte) :-
    Chan >= 0, Chan =< 15,
    ( Kind = two(off), Nib = 0x80
    ; Kind = two(on), Nib = 0x90
    ; Kind = two(kp), Nib = 0xA0
    ; Kind = two(cc), Nib = 0xB0
    ; Kind = one(pc), Nib = 0xC0
    ; Kind = one(cp), Nib = 0xD0
    ; Kind = two(pw), Nib = 0xE0
    ),
    Byte = Nib \/ Chan.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred byte2hex(int::in, hex::out, hex::out) is det.

byte2hex(Byte, MSN, LSN) :-
    ( if
        nibble2hex(Byte /\ 0xF, LSN0),
        nibble2hex((Byte >> 4) /\ 0xF, MSN0)
    then
        LSN = LSN0,
        MSN = MSN0
    else
        error("byte2hex: conversion failed!")
    ).

:- pred hex2byte(hex::in, hex::in, int::out) is det.

hex2byte(MSN, LSN, Byte) :-
    nibble2hex(A, MSN),
    nibble2hex(B, LSN),
    Byte = B \/ (A << 4).

:- pred nibble2hex(int, hex).
:- mode nibble2hex(in, out) is semidet.
:- mode nibble2hex(out, in) is det.

nibble2hex(0x0, x0).
nibble2hex(0x1, x1).
nibble2hex(0x2, x2).
nibble2hex(0x3, x3).
nibble2hex(0x4, x4).
nibble2hex(0x5, x5).
nibble2hex(0x6, x6).
nibble2hex(0x7, x7).
nibble2hex(0x8, x8).
nibble2hex(0x9, x9).
nibble2hex(0xA, xA).
nibble2hex(0xB, xB).
nibble2hex(0xC, xC).
nibble2hex(0xD, xD).
nibble2hex(0xE, xE).
nibble2hex(0xF, xF).

%-----------------------------------------------------------------------------%
:- end_module midi.
%-----------------------------------------------------------------------------%
