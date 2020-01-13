%-----------------------------------------------------------------------------%
% Copyright (C) 2005-2007 Peter Wang.
% Copyright (C) 2007 The University of Melbourne.
%-----------------------------------------------------------------------------%
%
% File: allegro.m.
% Author: wangp.
%
% A Mercury binding for Allegro.
%
%-----------------------------------------------------------------------------%

:- module allegro.
:- interface.

%
% The binding is divided into submodules which correspond with sections
% in the Allegro manual.
%

        % Using Allegro
    :- include_module allegro.init.

        % Mouse routines
    :- include_module allegro.mouse.

        % Timer routines
    :- include_module allegro.timer.

        % Keyboard routines
    :- include_module allegro.keyboard.

        % Joystick routines
    :- include_module allegro.joystick.

        % Graphics modes
    :- include_module allegro.graphics.

        % Bitmap objects
    :- include_module allegro.bitmap.

        % Loading image files
    :- include_module allegro.load_bitmap.
    :- include_module allegro.save_bitmap.

        % Palette routines
    :- include_module allegro.palette.

        % Truecolor pixel formats
    :- include_module allegro.color.

        % Drawing primitives
    :- include_module allegro.prim.

        % Blitting and sprites
    :- include_module allegro.blit.

        % RLE sprites
    :- include_module allegro.rle.

        % Compiled sprites
    :- include_module allegro.compiled.

        % Text output
    :- include_module allegro.text.

        % Transparency and patterned drawing
    :- include_module allegro.transparency.

        % Converting between color formats
    :- include_module allegro.color_format.

        % FLIC routines
    :- include_module allegro.flic.

        % Sound init routines
    :- include_module allegro.sound.

        % Mixer routines
    :- include_module allegro.mixer.

        % Digital sample routines
    :- include_module allegro.digi.

        % Music routines (MIDI)
    :- include_module allegro.midi.

        % Datafile routines
    :- include_module allegro.datafile.

        % Fixed point math routines
    :- include_module allegro.fixed.

        % 3D maths routines.
    :- include_module allegro.math3d.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- include_module allegro.util.

%
% Hooks for AllegroGL.
%
% We don't want the Allegro bindings to be dependent on AllegroGL, but we need
% to know the values of GFX_OPENGL_* for set_gfx_mode().  Initialisation of the
% AllegroGL bindings will set the following variables to the values of their
% respective constants.
%

:- pragma foreign_decl("C", "
    extern int __mal_GFX_OPENGL;
    extern int __mal_GFX_OPENGL_WINDOWED;
    extern int __mal_GFX_OPENGL_FULLSCREEN;
").

:- pragma foreign_code("C", "
    int __mal_GFX_OPENGL;
    int __mal_GFX_OPENGL_WINDOWED;
    int __mal_GFX_OPENGL_FULLSCREEN;
").

%-----------------------------------------------------------------------------%
% vi:ts=8:sts=4:sw=4:et
