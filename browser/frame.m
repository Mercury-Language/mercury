%---------------------------------------------------------------------------%
% Copyright (C) 1998-2000, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% frame - minimally implements ASCII graphics frames.
% This module is used by the term browser for displaying terms.
%
% XXX: This implementation is:
%	- very inefficient.
%	- specific to our immediate needs, and could be made more
%	  general.
%
% authors: aet
% stability: low

:- module mdb__frame.

:- interface.

:- import_module list, std_util.

	% XXX: Make frame type abstract instead?
% :- type frame.
:- type frame == list(string).

	% We always clip from top-left corner, hence only one pair of
	% coordinates is needed.
:- type frame__clip_rect == pair(int, int).

	% Width of a frame (horizontal size).
:- pred frame__hsize(frame, int).
:- mode frame__hsize(in, out) is det.

	% Height of a frame (vertical size).
:- pred frame__vsize(frame, int).
:- mode frame__vsize(in, out) is det.

	% Create a frame from a string.
:- pred frame__from_string(string, frame).
:- mode frame__from_string(in, out) is det.

	% Stack (vertically glue) two frames, left-aligned.
:- pred frame__vglue(frame, frame, frame).
:- mode frame__vglue(in, in, out) is det.

	% Juxtapose (horizontally glue) two frames, top-aligned.
:- pred frame__hglue(frame, frame, frame).
:- mode frame__hglue(in, in, out) is det.

	% clip a frame, where cliprect originates in top-left corner of frame.
:- pred frame__clip(frame__clip_rect, frame, frame).
:- mode frame__clip(in, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module string, list, int, io, require.
:- import_module mdb__util.

frame__from_string(Str, [Str]).

	% glue frames vertically (stack). align to left.
frame__vglue(TopFrame, BottomFrame, StackedFrame) :-
	list__append(TopFrame, BottomFrame, StackedFrame).

	% glue frames horizontally (juxtapose). align to top.
frame__hglue(LeftFrame, RightFrame, GluedFrame) :-
	frame__vsize(RightFrame, RVSize),
	frame__vsize(LeftFrame, LVSize),
	( RVSize < LVSize ->
		PadLines = LVSize - RVSize,
		frame_lower_pad(RightFrame, PadLines, RightFrameNew),
		LeftFrameNew = LeftFrame
	; LVSize < RVSize  ->
		PadLines = RVSize - LVSize,
		frame_lower_pad(LeftFrame, PadLines, LeftFrameNew),
		RightFrameNew = RightFrame
	;
		LeftFrameNew = LeftFrame,
		RightFrameNew = RightFrame
	),
	frame_right_pad(LeftFrameNew, PaddedLeftFrameNew),
	% XXX: mmc doesn't yet handle this. Use more verbose version instead.
	% zip_with(string__append, PaddedLeftFrameNew, RightFrameNew,
	%	GluedFrame).
	util__zip_with((pred(S1::in, S2::in, S3::out) is det :-
			string__append(S1,S2,S3)),
		PaddedLeftFrameNew, RightFrameNew, GluedFrame).

	% Add right padding. That is, add whitespace on right so that
	% lines are all equal length.
:- pred frame_right_pad(frame, frame).
:- mode frame_right_pad(in, out) is det.
frame_right_pad(Frame, PaddedFrame) :-
	Lengths = list__map((func(Str) = string__length(Str)), Frame),
	list__foldl(int__max, Lengths, 0, MaxLen),
	list__map(subtract(MaxLen), Lengths, Paddings),
	add_right_padding(Frame, Paddings, PaddedFrame).

:- pred add_right_padding(frame, list(int), frame).
:- mode add_right_padding(in, in, out) is det.
add_right_padding(Strs, Lens, PaddedFrame) :-
	( (Strs = [], Lens = []) ->
		PaddedFrame = []
	; (Strs = [S|Ss], Lens = [L|Ls]) ->
		list__duplicate(L, ' ', PadChars),
		string__from_char_list(PadChars, Padding),
		string__append(S, Padding, SP),
		add_right_padding(Ss, Ls, Rest),
		PaddedFrame = [SP|Rest]
	;
		error("add_right_padding: list arguments are of unequal length")
	).

	% We need this since Mercury has no Haskell-ese operation sections.
:- pred subtract(int, int, int).
:- mode subtract(in, in, out) is det.
subtract(M, X, Z) :-
	Z = M - X.

	% Add empty lines of padding to the bottom of a frame.
:- pred frame_lower_pad(frame, int, frame).
:- mode frame_lower_pad(in, in, out) is det.
frame_lower_pad(Frame, PadLines, PaddedFrame) :-
	list__duplicate(PadLines, "", Padding),
	list__append(Frame, Padding, PaddedFrame).

	% Horizontal size (width) of a frame
frame__hsize(Frame, HSize) :-
	Lengths = list__map(func(Str) = string__length(Str), Frame),
	list__foldl(int__max, Lengths, 0, MaxLen),
	HSize = MaxLen.

	% Vertical size (height) of a frame.
frame__vsize(Frame, VSize) :-
	length(Frame, VSize).

	% Clip a frame to the rectangle ((0,0),(X,Y)) where
	% origin is on the top-left. Coordinate axes go down and right.
frame__clip(X-Y, Frame, ClippedFrame) :-
	list__take_upto(Y, Frame, YClippedFrame),
	list__map(left(X), YClippedFrame, ClippedFrame).

:- pred left(int, string, string).
:- mode left(in, in, out) is det.
left(N, Str, Left) :-
	string__left(Str, N, Left).

:- pred frame__print(frame, io__state, io__state).
:- mode frame__print(in, di, uo) is det.
frame__print([]) -->
	{ true }.
frame__print([L|Ls]) -->
	io__write_string(L),
	io__nl,
	frame__print(Ls).

%---------------------------------------------------------------------------%
