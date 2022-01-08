%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003, 2005-2006 The University of Melbourne.
% Copyright (C) 2015, 2017-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: frame.m.
% Author: aet.
%
% frame - minimally implements ASCII graphics frames.
% This module is used by the term browser for displaying terms.
%
% XXX: This implementation is:
% - very inefficient.
% - specific to our immediate needs, and could be made more general.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module mdb.frame.
:- interface.

:- import_module list.
:- import_module pair.

%---------------------------------------------------------------------------%

    % XXX: Make frame type abstract instead?
:- type frame == list(string).

    % We always clip from top-left corner, hence only one pair of
    % coordinates is needed.
:- type clip_rect == pair(int, int).

    % Create a frame from a string.
    %
:- func from_string(string) = frame.

    % Width of a frame (horizontal size).
    %
:- func hsize(frame) = int.

    % Height of a frame (vertical size).
    %
:- func vsize(frame) = int.

    % Stack (vertically glue) two frames, left-aligned.
    %
:- func vglue(frame, frame) = frame.

    % Juxtapose (horizontally glue) two frames, top-aligned.
    %
:- func hglue(frame, frame) = frame.

    % Clip a frame to the rectangle ((0,0),(X,Y)) where origin is on the
    % top-left. Coordinate axes go down and right.
    %
:- func clip(clip_rect, frame) = frame.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module mdb.util.

:- import_module assoc_list.
:- import_module int.
:- import_module string.

%---------------------------------------------------------------------------%

from_string(Str) = [Str].

hsize(Frame) = HSize :-
    Lengths = list.map(func(Str) = string.length(Str), Frame),
    list.foldl(int.max, Lengths, 0, MaxLen),
    HSize = MaxLen.

vsize(Frame) = VSize :-
    length(Frame, VSize).

vglue(TopFrame, BottomFrame) = StackedFrame :-
    % Glue frames vertically (stack). Align to left.
    list.append(TopFrame, BottomFrame, StackedFrame).

hglue(LeftFrame, RightFrame) = GluedFrame :-
    % Glue frames horizontally (juxtapose). align to top.
    RVSize = vsize(RightFrame),
    LVSize = vsize(LeftFrame),
    ( if RVSize < LVSize then
        PadLines = LVSize - RVSize,
        RightFrameNew = frame_lower_pad(RightFrame, PadLines),
        LeftFrameNew = LeftFrame
    else if LVSize < RVSize  then
        PadLines = RVSize - LVSize,
        LeftFrameNew = frame_lower_pad(LeftFrame, PadLines),
        RightFrameNew = RightFrame
    else
        LeftFrameNew = LeftFrame,
        RightFrameNew = RightFrame
    ),
    frame_right_pad(LeftFrameNew, PaddedLeftFrameNew),
    util.zip_with(
        (pred(S1::in, S2::in, S3::out) is det :-
            string.append(S1, S2, S3)
        ), PaddedLeftFrameNew, RightFrameNew, GluedFrame).

    % Add right padding. That is, add whitespace on right so that
    % lines are all equal length.
    %
:- pred frame_right_pad(frame::in, frame::out) is det.

frame_right_pad(Frame, PaddedFrame) :-
    Lengths = list.map((func(Str) = string.length(Str)), Frame),
    list.foldl(int.max, Lengths, 0, MaxLen),
    list.map(subtract(MaxLen), Lengths, Paddings),
    list.map(add_right_padding,
        assoc_list.from_corresponding_lists(Frame, Paddings), PaddedFrame).

:- pred add_right_padding(pair(string, int)::in, string::out) is det.

add_right_padding(Str - Len, PaddedFrameStr) :-
    list.duplicate(Len, ' ', PadChars),
    string.from_char_list(PadChars, Padding),
    string.append(Str, Padding, PaddedFrameStr).

    % We need this since Mercury has no Haskell-ese operation sections.
    %
:- pred subtract(int::in, int::in, int::out) is det.

subtract(M, X, Z) :-
    Z = M - X.

    % Add empty lines of padding to the bottom of a frame.
    %
:- func frame_lower_pad(frame, int) = frame.

frame_lower_pad(Frame, PadLines) = PaddedFrame :-
    list.duplicate(PadLines, "", Padding),
    list.append(Frame, Padding, PaddedFrame).

clip(X-Y, Frame) = ClippedFrame :-
    list.take_upto(Y, Frame, YClippedFrame),
    list.map(left(X), YClippedFrame, ClippedFrame).

:- pred left(int::in, string::in, string::out) is det.

left(N, Str, Left) :-
    string.left(Str, N, Left).

%---------------------------------------------------------------------------%
:- end_module mdb.frame.
%---------------------------------------------------------------------------%
