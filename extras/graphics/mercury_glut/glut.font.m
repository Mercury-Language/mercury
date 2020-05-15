%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2007, 2012 The University of Melbourne.
% Copyright (C) 2017-2018, 2020 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%-----------------------------------------------------------------------------%
%
% File: glut.font.m.
% Author: juliensf.
%
% This module provides an interface to the GLUT font API.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module glut.font.
:- interface.

:- import_module char.

%----------------------------------------------------------------------------%
%
% Bitmap fonts
%

:- type bitmap_font
    --->    bitmap_8_by_13
    ;       bitmap_9_by_15
    ;       times_roman_10
    ;       times_roman_24
    ;       helvetica_10
    ;       helvetica_12
    ;       helvetica_18.

    % Render a bitmap character using OpenGL.  Does not use display
    % lists.  Adjusts the current raster position based upon
    % the width of the character.
    %
:- pred font.bitmap_character(bitmap_font::in, char::in,
    io::di, io::uo) is det.

    % Return the width of the character in pixels when rendered
    % using the specified font.
    %
:- func font.bitmap_width(bitmap_font, char) = int.

    % Return the length of the string in pixels when rendered using
    % the specified font.
    %
:- func font.bitmap_length(bitmap_font, string) = int.

%----------------------------------------------------------------------------%
%
% Stroke fonts
%

:- type stroke_font
    --->    roman
    ;       mono_roman.

    % Render a stroke character using OpenGL.
    %
:- pred font.stroke_character(stroke_font::in, char::in, io::di,
    io::uo) is det.

    % Return the width of the character in pixels when rendered
    % using the specified font.
    %
:- func font.stroke_width(stroke_font, char) = int.

    % Return the length of the string in pixels when rendered using
    % the specified font.
    %
:- func font.stroke_length(stroke_font, string) = int.

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
:- pragma foreign_type("C", font_ptr, "void *", [can_pass_as_mercury_type]).

%----------------------------------------------------------------------------%
%
% Bitmap fonts
%

font.bitmap_character(Font, Char, !IO) :-
    bitmap_character_2(bitmap_font_to_ptr(Font), Char, !IO).

:- pred bitmap_character_2(font_ptr::in, char::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    bitmap_character_2(FntPtr::in, C::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutBitmapCharacter(FntPtr, (int) C);
").

font.bitmap_width(Font, Char) = Width :-
    bitmap_width_2(bitmap_font_to_ptr(Font), Char, Width).

:- pred bitmap_width_2(font_ptr::in, char::in, int::out) is det.
:- pragma foreign_proc("C",
    bitmap_width_2(FntPtr::in, C::in, Width::out),
    [will_not_call_mercury, promise_pure],
"
    Width = (MR_Integer) glutBitmapWidth(FntPtr, (int) C);
").

font.bitmap_length(Font, String) = Length :-
    bitmap_length_2(bitmap_font_to_ptr(Font), String, Length).

:- pred bitmap_length_2(font_ptr::in, string::in, int::out) is det.
:- pragma foreign_proc("C",
    bitmap_length_2(FntPtr::in, Str::in, Length::out),
    [will_not_call_mercury, promise_pure],
"
    Length =  (MR_Integer) glutBitmapLength(FntPtr, Str);
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
% Stroke fonts
%

font.stroke_character(Font, Char, !IO) :-
    stroke_character_2(stroke_font_to_ptr(Font), Char, !IO).

:- pred stroke_character_2(font_ptr::in, char::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
    stroke_character_2(StrokeFntPtr::in, C::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutStrokeCharacter(StrokeFntPtr, (int) C);
").

font.stroke_width(Font, Char) = Width :-
    stroke_width_2(stroke_font_to_ptr(Font), Char, Width).

:- pred stroke_width_2(font_ptr::in, char::in, int::out) is det.
:- pragma foreign_proc("C",
    stroke_width_2(StrokeFntPtr::in, C::in, Width::out),
    [will_not_call_mercury, promise_pure],
"
    Width = (MR_Integer) glutStrokeWidth(StrokeFntPtr, (int) C);
").

font.stroke_length(Font, String) = Length :-
    stroke_length_2(stroke_font_to_ptr(Font), String, Length).

:- pred stroke_length_2(font_ptr::in, string::in, int::out) is det.
:- pragma foreign_proc("C",
    stroke_length_2(StrokeFntPtr::in, Str::in, Length::out),
    [will_not_call_mercury, promise_pure],
"
    Length = (MR_Integer) glutStrokeLength(StrokeFntPtr, Str);
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
