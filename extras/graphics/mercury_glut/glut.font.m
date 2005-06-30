%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: glut.font.m
% author: juliensf
%
% This module provides an interface to the GLUT font API.
%
%-----------------------------------------------------------------------------%

:- module glut.font.

:- interface.

%----------------------------------------------------------------------------%
% 
% Bitmap fonts.
%

:- type bitmap_font
	--->	bitmap_8_by_13	
	;	bitmap_9_by_15	
	;	times_roman_10	
	;	times_roman_24
	;	helvetica_10
	;	helvetica_12
	;	helvetica_18.

	% Render a bitmap character using OpenGL.  Does not use display
	% lists.  Adjusts the current raster position based upon
	% the width of the character.
	%
:- pred font.bitmap_character(bitmap_font::in, char::in, io::di,
	io::uo) is det.

	% Return the width of the character in pixels when rendered
	% using the specified font.
	%
:- pred font.bitmap_width(bitmap_font::in, char::in, int::out, io::di,
	io::uo) is det.

	% Return the length of the string in pixels when rendered using
	% the specified font.
	%
:- pred font.bitmap_length(bitmap_font::in, string::in, int::out, io::di,
	io::uo) is det.

%----------------------------------------------------------------------------%
%
% Stroke fonts.
%

:- type stroke_font
	--->	roman
	;	mono_roman.

	% Render a stroke character using OpenGL.
	%
:- pred font.stroke_character(stroke_font::in, char::in, io::di,
	io::uo) is det.

	% Return the width of the character in pixels when rendered 
	% using the specified font.
	%
:- pred font.stroke_width(stroke_font::in, char::in, int::out, io::di,

	io::uo) is det.

	% Return the length of the string in pixels when rendered using
	% the specified font.
	%
:- pred font.stroke_length(stroke_font::in, string::in, int::out, io::di,
	io::uo) is det.

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C",
"
	#if defined(__APPLE__) && defined(__MACH__)
		#include <GLUT/glut.h>
	#else
		#include <GL/glut.h>
	#endif
").

:- type font_ptr.
:- pragma foreign_type("C", font_ptr, "void *").

%----------------------------------------------------------------------------%
%
% Bitmap fonts.
%

font.bitmap_character(Font, Char, !IO) :-
	bitmap_character_2(bitmap_font_to_ptr(Font), Char, !IO).

:- pred bitmap_character_2(font_ptr::in, char::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
	bitmap_character_2(FntPtr::in, C::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutBitmapCharacter(FntPtr, (int) C);
	IO = IO0;
").

font.bitmap_width(Font, Char, Width, !IO) :-
	bitmap_width_2(bitmap_font_to_ptr(Font), Char, Width, !IO).

:- pred bitmap_width_2(font_ptr::in, char::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
	bitmap_width_2(FntPtr::in, C::in, Width::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	Width = (MR_Integer) glutBitmapWidth(FntPtr, (int) C);
	IO = IO0;
").

font.bitmap_length(Font, String, Length, !IO) :-
	bitmap_length_2(bitmap_font_to_ptr(Font), String, Length, !IO).

:- pred bitmap_length_2(font_ptr::in, string::in, int::out, io::di, io::uo)
	is det.
:- pragma foreign_proc("C",
	bitmap_length_2(FntPtr::in, Str::in, Length::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	Length =  (MR_Integer) glutBitmapLength(FntPtr, Str);
	IO = IO0;
").

:- func bitmap_font_to_ptr(bitmap_font) = font_ptr.

bitmap_font_to_ptr(bitmap_8_by_13) = bitmap_8_by_13_ptr.
bitmap_font_to_ptr(bitmap_9_by_15) = bitmap_9_by_15_ptr.
bitmap_font_to_ptr(times_roman_10) = times_roman_10_ptr.
bitmap_font_to_ptr(times_roman_24) = times_roman_24_ptr.
bitmap_font_to_ptr(helvetica_10)   = helvetica_10_ptr.
bitmap_font_to_ptr(helvetica_12)   = helvetica_12_ptr.
bitmap_font_to_ptr(helvetica_18)   = helvetica_18_ptr.

:- func bitmap_8_by_13_ptr = font_ptr.
:- pragma foreign_proc("C",
	bitmap_8_by_13_ptr = (FntPtr::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	FntPtr = GLUT_BITMAP_8_BY_13;
").

:- func bitmap_9_by_15_ptr = font_ptr.
:- pragma foreign_proc("C",
	bitmap_9_by_15_ptr = (FntPtr::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	FntPtr = GLUT_BITMAP_9_BY_15;
").

:- func times_roman_10_ptr = font_ptr.
:- pragma foreign_proc("C",
	times_roman_10_ptr = (FntPtr::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	FntPtr = GLUT_BITMAP_TIMES_ROMAN_10;
").

:- func times_roman_24_ptr = font_ptr.
:- pragma foreign_proc("C",
	times_roman_24_ptr = (FntPtr::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	FntPtr = GLUT_BITMAP_TIMES_ROMAN_24;
").

:- func helvetica_10_ptr = font_ptr.
:- pragma foreign_proc("C",
	helvetica_10_ptr = (FntPtr::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	FntPtr = GLUT_BITMAP_HELVETICA_10;
").

:- func helvetica_12_ptr = font_ptr.
:- pragma foreign_proc("C",
	helvetica_12_ptr = (FntPtr::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	FntPtr = GLUT_BITMAP_HELVETICA_12;
").

:- func helvetica_18_ptr = font_ptr.
:- pragma foreign_proc("C",
	helvetica_18_ptr = (FntPtr::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	FntPtr = GLUT_BITMAP_HELVETICA_18;
").

%----------------------------------------------------------------------------%
%
% Stroke fonts.
%

font.stroke_character(Font, Char, !IO) :-
	stroke_character_2(stroke_font_to_ptr(Font), Char, !IO).

:- pred stroke_character_2(font_ptr::in, char::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
	stroke_character_2(StrokeFntPtr::in, C::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutStrokeCharacter(StrokeFntPtr, (int) C);
	IO = IO0;
").

font.stroke_width(Font, Char, Width, !IO) :-
	stroke_width_2(stroke_font_to_ptr(Font), Char, Width, !IO).

:- pred stroke_width_2(font_ptr::in, char::in, int::out, io::di,
	io::uo) is det.
:- pragma foreign_proc("C", 
	stroke_width_2(StrokeFntPtr::in, C::in, Width::out, IO0::di,
		IO::uo),
	[will_not_call_mercury, promise_pure],
"
	Width = (MR_Integer) glutStrokeWidth(StrokeFntPtr, (int) C);
	IO = IO0;
").

font.stroke_length(Font, String, Length, !IO) :-
	stroke_length_2(stroke_font_to_ptr(Font), String, Length, !IO).

:- pred stroke_length_2(font_ptr::in, string::in, int::out,
	io::di, io::uo) is det.
:- pragma foreign_proc("C",
	stroke_length_2(StrokeFntPtr::in, Str::in, Length::out,
		IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	Length = (MR_Integer) glutStrokeLength(StrokeFntPtr, Str);
	IO = IO0;
").

:- func stroke_font_to_ptr(stroke_font) = font_ptr.

stroke_font_to_ptr(roman) = stroke_roman_ptr.
stroke_font_to_ptr(mono_roman) = stroke_mono_roman_ptr.

:- func stroke_roman_ptr = font_ptr.
:- pragma foreign_proc("C",
	stroke_roman_ptr = (FntPtr::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	FntPtr = GLUT_STROKE_ROMAN;
").

:- func stroke_mono_roman_ptr = font_ptr.
:- pragma foreign_proc("C",
	stroke_mono_roman_ptr = (FntPtr::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	FntPtr = GLUT_STROKE_MONO_ROMAN;
").

%----------------------------------------------------------------------------%
:- end_module glut.font.
%----------------------------------------------------------------------------%
