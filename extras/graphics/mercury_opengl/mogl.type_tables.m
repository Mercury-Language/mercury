%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1997, 2003-2006 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: mogl.type_tables.m.
% Main authors: conway, juliensf.
%
% This file exposes some of the <type>_to_int and <type>_flags[] tables.
% Modules other than mogl can import this module to convert mogl types to
% their equivalent OpenGL constants.  The interface is subject to change.
%
%------------------------------------------------------------------------------%

:- module mogl.type_tables.
:- interface.

:- func pixel_format_to_int(pixel_format) = int.

:- func pixel_type_to_int(pixel_type) = int.

:- pred texture_format_to_int(texture_format, int).
:- mode texture_format_to_int(in, out) is det.
%:- mode texture_format_to_int(out, in) is semidet.

%-----------------------------------------------------------------------------%

:- implementation.

%------------------------------------------------------------------------------%

    % XXX Check that this works on Windows.
    % We may need to #include <windows.h> to make it work.
:- pragma foreign_decl("C", "
    #include <stdio.h>
    #include <math.h>
    #include <assert.h>
    
    #if defined(__APPLE__) && defined(__MACH__)
        #include <OpenGL/gl.h>
    #else
        #include <GL/gl.h>
    #endif
").

%------------------------------------------------------------------------------%

pixel_format_to_int(color_index)     = 0.
pixel_format_to_int(stencil_index)   = 1.
pixel_format_to_int(depth_component) = 2.
pixel_format_to_int(red)             = 3.
pixel_format_to_int(green)           = 4.
pixel_format_to_int(blue)            = 5.
pixel_format_to_int(alpha)           = 6.
pixel_format_to_int(rgb)             = 7.
pixel_format_to_int(rgba)            = 8.
pixel_format_to_int(luminance)       = 9.
pixel_format_to_int(luminance_alpha) = 10.

:- pragma foreign_decl("C", "
    extern const GLenum pixel_format_flags[];
").

:- pragma foreign_code("C", "
    const GLenum pixel_format_flags[] = {
        GL_COLOR_INDEX,
        GL_STENCIL_INDEX,
        GL_DEPTH_COMPONENT,
        GL_RED,
        GL_GREEN,
        GL_BLUE,
        GL_ALPHA,
        GL_RGB,
        GL_RGBA,
        GL_LUMINANCE,
        GL_LUMINANCE_ALPHA
    };
").

%------------------------------------------------------------------------------%

pixel_type_to_int(unsigned_byte)  = 0.
pixel_type_to_int(bitmap)         = 1.
pixel_type_to_int(byte)           = 2.
pixel_type_to_int(unsigned_short) = 3.
pixel_type_to_int(unsigned_int)   = 4.
pixel_type_to_int(int)            = 5.
pixel_type_to_int(float)          = 6.

:- pragma foreign_decl("C", "
    extern const GLenum pixel_type_flags[];
").

:- pragma foreign_code("C",
"
    const GLenum pixel_type_flags[] = {
        GL_UNSIGNED_BYTE,
        GL_BITMAP,
        GL_BYTE,
        GL_UNSIGNED_SHORT,
        GL_UNSIGNED_INT,
        GL_INT,
        GL_FLOAT
    };
").

%------------------------------------------------------------------------------%

:- pragma foreign_code("C", "
    const GLenum texture_format_flags[] = {
        GL_ALPHA,
        GL_LUMINANCE,
        GL_LUMINANCE_ALPHA,
        GL_INTENSITY,
        GL_RGB,
        GL_RGBA,
        GL_ALPHA4,
        GL_ALPHA8,
        GL_ALPHA12,
        GL_ALPHA16,
        GL_LUMINANCE4,
        GL_LUMINANCE8,
        GL_LUMINANCE12,
        GL_LUMINANCE16,
        GL_LUMINANCE4_ALPHA4,
        GL_LUMINANCE6_ALPHA2,
        GL_LUMINANCE8_ALPHA8,
        GL_LUMINANCE12_ALPHA4,
        GL_LUMINANCE12_ALPHA12,
        GL_LUMINANCE16_ALPHA16,
        GL_INTENSITY4,
        GL_INTENSITY8,
        GL_INTENSITY12,
        GL_INTENSITY16,
        GL_R3_G3_B2,
        GL_RGB4,
        GL_RGB5,
        GL_RGB10,
        GL_RGB12,
        GL_RGB16,
        GL_RGBA2,
        GL_RGBA4,
        GL_RGB5_A1,
        GL_RGBA8,
        GL_RGB10_A2,
        GL_RGBA12,
        GL_RGBA16
    };
").

texture_format_to_int(alpha, 0).        
texture_format_to_int(luminance, 1).
texture_format_to_int(luminance_alpha, 2).
texture_format_to_int(intensity, 3).
texture_format_to_int(rgb, 4).
texture_format_to_int(rgba, 5).
texture_format_to_int(alpha4, 6).
texture_format_to_int(alpha8, 7).
texture_format_to_int(alpha12, 8).
texture_format_to_int(alpha16, 9).
texture_format_to_int(luminance4, 10).
texture_format_to_int(luminance8, 11).
texture_format_to_int(luminance12, 12).
texture_format_to_int(luminance16, 13).
texture_format_to_int(luminance4_alpha4, 14).
texture_format_to_int(luminance6_alpha2, 15).
texture_format_to_int(luminance8_alpha8, 16).
texture_format_to_int(luminance12_alpha4, 17).
texture_format_to_int(luminance12_alpha12, 18).
texture_format_to_int(luminance16_alpha16, 19).
texture_format_to_int(intensity4, 20).
texture_format_to_int(intensity8, 21).
texture_format_to_int(intensity12, 22).
texture_format_to_int(intensity16, 23).
texture_format_to_int(r3_g3_b2, 24).
texture_format_to_int(rgb4, 25).
texture_format_to_int(rgb5, 26).
texture_format_to_int(rgb10, 27).
texture_format_to_int(rgb12, 28).
texture_format_to_int(rgb16, 29).
texture_format_to_int(rgba2, 30).
texture_format_to_int(rgba4, 31).
texture_format_to_int(rgb5_a1, 32).
texture_format_to_int(rgba8, 33).
texture_format_to_int(rgb10_a2, 34).
texture_format_to_int(rgba12, 35).
texture_format_to_int(rgba16, 36).

%------------------------------------------------------------------------------%
:- end_module mogl.type_tables.
%------------------------------------------------------------------------------%
