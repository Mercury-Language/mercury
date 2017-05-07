%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2007 The University of Melbourne.
% Copyright (C) 2017 The Mercury team.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: glut.callback.m.
% Author: juliensf.
%
% This module contains predicates for (un)registering glut callbacks.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module glut.callback.
:- interface.

%-----------------------------------------------------------------------------%

    % Registers the display callback for the current window.
    % This is called whenever GLUT determines that the window needs
    % to be redisplayed.  You can call glut.window.post_redisplay/2
    % to force the window to be redrawn.
    %
:- pred callback.display_func(pred(io, io), io, io).
:- mode callback.display_func(pred(di, uo) is det, di, uo)is det.

    % Unregisters the display callback for the current window.
    %
:- pred callback.disable_display_func(io::di, io::uo) is det.

    % Registers the reshape callback for the current window.
    % The reshape callback is Reshape(Width, Height, !IO).
    % `Width' and `Height' specify the new window size measured
    % in pixels.
    %
:- pred callback.reshape_func(pred(int, int, io, io), io, io).
:- mode callback.reshape_func(pred(in, in, di, uo) is det, di, uo) is det.

    % Unregisters the reshape callback for the current window.
    %
:- pred callback.disable_reshape_func(io::di, io::uo) is det.

    % Registers the keyboard callback for the current window.
    % This is called whenever a key is pressed.
    % The keyboard callback is Keyboard(Key, X, Y, !IO).  `Key'
    % is the ASCII value of the key pressed.  `X' and `Y' are the
    % mouse coordinates at the time the key is pressed.
    %
:- pred callback.keyboard_func(pred(char, int, int, io, io), io, io).
:- mode callback.keyboard_func(pred(in, in, in, di, uo) is det, di, uo) is det.

    % Unregisters the keyboard callback for the current window.
    %
:- pred callback.disable_keyboard_func(io::di, io::uo) is det.

:- type button
    --->    left
    ;       middle
    ;       right.

:- type button_state
    --->    up
    ;       down.

    % Registers the mouse callback for the current window.
    % This is called whenever the state of one of the mouse buttons
    % changes (i.e.. a button is pressed or released).  The mouse
    % callback is MouseFunc(Button, State, X, Y, !IO).  `Button'
    % is the identity of the button, `State' indicates whether the
    % button was pressed or released.  `X' and `Y' give the mouse pointer
    % coordinates at the time of the button event.
    %
:- pred callback.mouse_func(pred(button, button_state, int, int, io, io),
    io, io).
:- mode callback.mouse_func(pred(in, in, in, in, di, uo) is det, di, uo)
    is det.

    % Unregisters the mouse callback for the current window.
    %
:- pred callback.disable_mouse_func(io::di, io::uo) is det.

    % Registers the mouse motion callback for the current window.
    % The motion callback is called if the mouse is moved while
    % one of the buttons is pressed.
    %
:- pred callback.motion_func(pred(int, int, io, io), io, io).
:- mode callback.motion_func(pred(in, in, di, uo) is det, di, uo) is det.

    % Unregisters the mouse motion callback for the current window.
    %
:- pred callback.disable_motion_func(io::di, io::uo) is det.

    % Registers the passive motion callback for the current window.
    % The passive motion callback is called if the mouse is moved while
    % no mouse buttons are pressed.
    %
:- pred callback.passive_motion_func(pred(int, int, io, io), io, io).
:- mode callback.passive_motion_func(pred(in, in, di, uo) is det, di, uo)
    is det.

    % Unregisters the passive motion callback for the current window.
    %
:- pred callback.disable_passive_motion_func(io::di, io::uo) is det.

:- type entry_state
    --->    left
    ;       entered.

    % Registers the entry callback for the current window.
    % This is called whenever the mouse pointer enters/leaves the
    % current window.
    %
:- pred callback.entry_func(pred(entry_state, io, io), io, io).
:- mode callback.entry_func(pred(in, di, uo) is det, di, uo) is det.

    % Unregisters the entry callback for the current window.
    %
:- pred callback.disable_entry_func(io::di, io::uo) is det.

:- type visibility
    --->    visible
    ;       not_visible.

    % Register the visibility callback for the current window.
    % This visibility callback is whenever the visibility of a
    % window changes.
    %
:- pred callback.visibility_func(pred(visibility, io, io), io, io).
:- mode callback.visibility_func(pred(in, di, uo) is det, di, uo) is det.

    % Unregister the visibility callback for the current window.
    %
:- pred callback.disable_visibility_func(io::di, io::uo) is det.

    % Register the global idle callback.  The idle callback is called
    % continuously when the are no other events to be processed.
    %
:- pred callback.idle_func(pred(io, io), io, io).
:- mode callback.idle_func(pred(di, uo) is det, di, uo) is det.

    % Unregister the global idle callback.
    %
:- pred callback.disable_idle_func(io::di, io::uo) is det.

    % NYI.
%:- pred callback.timer_func(pred(io, io), int).
%:- mode callback.timer_func(pred(di, uo) is det.

:- type special_key
    --->    f1
    ;       f2
    ;       f3
    ;       f4
    ;       f5
    ;       f6
    ;       f7
    ;       f8
    ;       f9
    ;       f10
    ;       f11
    ;       f12
    ;       left
    ;       up
    ;       right
    ;       down
    ;       page_up
    ;       page_down
    ;       home
    ;       end
    ;       insert.

    % Register the special keyboard callback for the current window.
    % This is called when one of the function or arrow keys is pressed.
    %
:- pred callback.special_func(pred(special_key, int, int, io, io), io, io).
:- mode callback.special_func(pred(in, in, in, di, uo) is det, di, uo) is det.

    % Unregister the special keyboard callback for the current window.
    %
:- pred callback.disable_special_func(io::di, io::uo) is det.

    % Register the special keyboard up callback for the current window.
    % This is called when one of the function or arrow keys is released.
    %
:- pred callback.special_up_func(pred(special_key, int, int, io, io), io, io).
:- mode callback.special_up_func(pred(in, in, in, di, uo) is det, di, uo)
    is det.

    % Unregister the special keyboard up callback for the current window.
    %
:- pred callback.disable_special_up_func(io::di, io::uo) is det.

    % Register the keyboard_up callback for the current window.
    % This is called whenever a key is released.  The arguments
    % of the callback predicate are the same as the keyboard callback.
    %
:- pred callback.keyboard_up_func(pred(char, int, int, io, io), io, io).
:- mode callback.keyboard_up_func(pred(in, in, in, di, uo) is det, di, uo)
    is det.

    % Unregister the keyboard_up callback for the current window.
    %
:- pred callback.disable_keyboard_up_func(io::di, io::uo) is det.

    % Register the overlay display callback for the current window.
    % This is called whenever GLUT determines that the overlay plane
    % for the current window needs to be redrawn.
    %
:- pred callback.overlay_display_func(pred(io, io), io, io).
:- mode callback.overlay_display_func(pred(di, uo) is det, di, uo) is det.

    % Unregister the overlay display callback for the current window.
    %
:- pred callback.overlay_display_func(io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C",
"
    #include <stdio.h>

    #if defined(__APPLE__) && defined(__MACH__)
        #include <GLUT/glut.h>
    #else
        #include <GL/glut.h>
    #endif
").

%-----------------------------------------------------------------------------%

    % Global callbacks.
    %
:- pragma foreign_decl("C", "
    void MGLUT_idle_callback(void);

    extern MR_Word mglut_idle_callback;
").

    % Window specific callbacks.
    %
:- pragma foreign_decl("C", "

    void MGLUT_display_callback(void);
    void MGLUT_reshape_callback(int, int);
    void MGLUT_keyboard_callback(unsigned char, int, int);
    void MGLUT_mouse_callback(int, int, int, int);
    void MGLUT_motion_callback(int, int);
    void MGLUT_passive_motion_callback(int, int);
    void MGLUT_entry_callback(int);
    void MGLUT_visibility_callback(int);
    void MGLUT_special_callback(int, int, int);
    void MGLUT_special_up_callback(int, int, int);
    void MGLUT_keyboard_up_callback(unsigned char, int, int);
    void MGLUT_overlay_display_callback(void);

    extern MR_Word mglut_display_callback;
    extern MR_Word mglut_reshape_callback;
    extern MR_Word mglut_keyboard_callback;
    extern MR_Word mglut_mouse_callback;
    extern MR_Word mglut_entry_callback;
    extern MR_Word mglut_visibility_callback;
    extern MR_Word mglut_motion_callback;
    extern MR_Word mglut_passive_motion_callback;
    extern MR_Word mglut_special_callback;
    extern MR_Word mglut_special_up_callback;
    extern MR_Word mglut_keyboard_up_callback;
    extern MR_Word mglut_overlay_display_callback;
").

:- pragma foreign_code("C", "

    /*
    ** XXX If we ever support multiple windows remember that the idle
    ** callback is global.
    */
    MR_Word mglut_idle_callback;

    MR_Word mglut_display_callback;
    MR_Word mglut_reshape_callback;
    MR_Word mglut_keyboard_callback;
    MR_Word mglut_mouse_callback;
    MR_Word mglut_motion_callback;
    MR_Word mglut_passive_motion_callback;
    MR_Word mglut_entry_callback;
    MR_Word mglut_visibility_callback;
    MR_Word mglut_special_callback;
    MR_Word mglut_special_up_callback;
    MR_Word mglut_keyboard_up_callback;
    MR_Word mglut_overlay_display_callback;
").

%-----------------------------------------------------------------------------%
%
% Display callbacks
%

:- pragma foreign_proc("C",
    callback.display_func(DisplayFunc::pred(di, uo) is det,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_display_callback = DisplayFunc;
    glutDisplayFunc(MGLUT_display_callback);
").

:- pragma foreign_code("C", "
void MGLUT_display_callback(void)
{
    MGLUT_do_display_callback(mglut_display_callback);
}
").

:- pragma foreign_export("C",
    do_display_callback(pred(di, uo) is det, di, uo),
    "MGLUT_do_display_callback").
:- pred do_display_callback(pred(io, io), io, io).
:- mode do_display_callback(pred(di, uo) is det, di, uo) is det.

do_display_callback(DisplayFunc, !IO) :- DisplayFunc(!IO).

:- pragma foreign_proc("C",
    disable_display_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutDisplayFunc(NULL);
").

%-----------------------------------------------------------------------------%
%
% Reshape callbacks
%

:- pragma foreign_proc("C",
    callback.reshape_func(Reshape::pred(in, in, di, uo) is det,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_reshape_callback = Reshape;
    glutReshapeFunc(MGLUT_reshape_callback);
").

:- pragma foreign_code("C", "
void MGLUT_reshape_callback(int width, int height)
{
    MGLUT_do_reshape_callback(mglut_reshape_callback, width, height);
}
").

:- pragma foreign_export("C",
    do_reshape_callback(pred(in, in, di, uo) is det, in, in, di, uo),
    "MGLUT_do_reshape_callback").
:- pred do_reshape_callback(pred(int, int, io, io), int, int, io, io).
:- mode do_reshape_callback(pred(in, in, di, uo) is det, in, in, di, uo) is det.

do_reshape_callback(ReshapeFunc, Width, Height, !IO) :-
    ReshapeFunc(Width, Height, !IO).

:- pragma foreign_proc("C",
    disable_reshape_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutReshapeFunc(NULL);
").

%-----------------------------------------------------------------------------%
%
% Keyboard callbacks
%

:- pragma foreign_proc("C",
    keyboard_func(KeyboardFunc::pred(in, in, in, di, uo) is det, _IO0::di,
        _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_keyboard_callback = KeyboardFunc;
    glutKeyboardFunc(MGLUT_keyboard_callback);
").

:- pragma foreign_code("C", "
void MGLUT_keyboard_callback(unsigned char scan_code, int x, int y)
{
    MGLUT_do_keyboard_callback(mglut_keyboard_callback,
        (MR_Char) scan_code, (MR_Integer) x, (MR_Integer) y);
}
").

:- pragma foreign_export("C",
    do_keyboard_callback(pred(in, in, in, di, uo) is det, in, in, in, di, uo),
    "MGLUT_do_keyboard_callback").
:- pred do_keyboard_callback(pred(char, int, int, io, io), char, int, int,
    io, io).
:- mode do_keyboard_callback(pred(in, in, in, di, uo) is det, in, in, in,
    di, uo) is det.

do_keyboard_callback(KeyBoardFunc, ScanCode, X, Y, !IO) :-
    KeyBoardFunc(ScanCode, X, Y, !IO).

:- pragma foreign_proc("C",
    disable_keyboard_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutKeyboardFunc(NULL);
").

%-----------------------------------------------------------------------------%
%
% Mouse callbacks
%

:- pragma foreign_proc("C",
    mouse_func(MouseFunc::pred(in, in, in, in, di, uo) is det,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_mouse_callback = MouseFunc;
    glutMouseFunc(MGLUT_mouse_callback);
").

:- pragma foreign_code("C", "
void MGLUT_mouse_callback(int button, int state, int x, int y)
{
    MGLUT_do_mouse_callback(mglut_mouse_callback, (MR_Integer) button,
        (MR_Integer) state, (MR_Integer) x, (MR_Integer) y);
}
").

:- pragma foreign_export("C",
    do_mouse_callback(pred(in, in, in, in, di, uo) is det,
        in, in, in, in, di, uo),
    "MGLUT_do_mouse_callback").
:- pred do_mouse_callback(pred(button, button_state, int, int, io, io),
        button, button_state, int, int, io, io).
:- mode do_mouse_callback(pred(in, in, in, in, di, uo) is det, in, in, in,
        in, di, uo) is det.

do_mouse_callback(MouseFunc, Button, ButtonState, X, Y, !IO) :-
    MouseFunc(Button, ButtonState, X, Y, !IO).

:- pragma foreign_enum("C", button/0,
[
    left    - "GLUT_LEFT_BUTTON",
    middle  - "GLUT_MIDDLE_BUTTON",
    right   - "GLUT_RIGHT_BUTTON"
]).

:- pragma foreign_enum("C", button_state/0,
[
    up   - "GLUT_UP",
    down - "GLUT_DOWN"
]).

:- pragma foreign_proc("C",
    disable_mouse_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, tabled_for_io],
"
    glutMouseFunc(NULL);
").

%-----------------------------------------------------------------------------%
%
% Motion callback
%

:- pragma foreign_proc("C",
    motion_func(MotionFunc::pred(in, in, di, uo) is det, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_motion_callback = MotionFunc;
    glutMotionFunc(MGLUT_motion_callback);
").

:- pragma foreign_code("C", "
void MGLUT_motion_callback(int x, int y)
{
    MGLUT_do_motion_callback(mglut_motion_callback, (MR_Integer) x,
        (MR_Integer) y);
}
").

:- pragma foreign_export("C",
    do_motion_callback(pred(in, in, di, uo) is det, in, in, di, uo),
    "MGLUT_do_motion_callback").
:- pred do_motion_callback(pred(int, int, io, io), int, int, io, io).
:- mode do_motion_callback(pred(in, in, di, uo) is det, in, in, di, uo) is det.

do_motion_callback(MotionFunc, X, Y, !IO) :- MotionFunc(X, Y, !IO).

:- pragma foreign_proc("C",
    disable_motion_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    glutMotionFunc(NULL);
").

%-----------------------------------------------------------------------------%
%
% Passive motion callbacks
%

:- pragma foreign_proc("C",
    passive_motion_func(PassiveMotionFunc::pred(in, in, di, uo) is det,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_passive_motion_callback = PassiveMotionFunc;
    glutPassiveMotionFunc(MGLUT_passive_motion_callback);
").

:- pragma foreign_code("C", "
void MGLUT_passive_motion_callback(int x, int y)
{
    MGLUT_do_passive_motion_callback(mglut_passive_motion_callback,
        (MR_Integer) x, (MR_Integer) y);
}
").

:- pragma foreign_export("C",
    do_passive_motion_callback(pred(in, in, di, uo) is det,
        in, in, di, uo),
    "MGLUT_do_passive_motion_callback").
:- pred do_passive_motion_callback(pred(int, int, io, io), int, int, io, io).
:- mode do_passive_motion_callback(pred(in, in, di, uo) is det, in, in,
    di, uo) is det.

do_passive_motion_callback(PassiveMotionFunc, X, Y, !IO) :-
    PassiveMotionFunc(X, Y, !IO).

:- pragma foreign_proc("C",
    disable_passive_motion_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutPassiveMotionFunc(NULL);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    entry_func(EntryFunc::pred(in, di, uo) is det, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_entry_callback = EntryFunc;
    glutEntryFunc(MGLUT_entry_callback);
").

:- pragma foreign_code("C", "
void MGLUT_entry_callback(int state)
{
    MGLUT_do_entry_callback(mglut_entry_callback, state);
}").

:- pragma foreign_export("C",
    do_entry_callback(pred(in, di, uo) is det, in, di, uo),
    "MGLUT_do_entry_callback").
:- pred do_entry_callback(pred(entry_state, io, io), entry_state, io, io).
:- mode do_entry_callback(pred(in, di, uo) is det, in, di, uo) is det.

do_entry_callback(EntryFunc, EntryState, !IO) :-
    EntryFunc(EntryState, !IO).

:- pragma foreign_proc("C",
    disable_entry_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutEntryFunc(NULL);
").

:- pragma foreign_enum("C", entry_state/0,
[
    left    - "GLUT_LEFT",
    entered - "GLUT_ENTERED"
]).

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    visibility_func(VisibilityFunc::pred(in, di, uo) is det, _IO0::di,
        _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_visibility_callback = VisibilityFunc;
    glutVisibilityFunc(MGLUT_visibility_callback);
").

:- pragma foreign_code("C", "
void MGLUT_visibility_callback(int state)
{
    MGLUT_do_visibility_callback(mglut_visibility_callback, state);
}").

:- pragma foreign_export("C",
    do_visibility_callback(pred(in, di, uo) is det, in, di, uo),
    "MGLUT_do_visibility_callback").
:- pred do_visibility_callback(pred(visibility, io, io), visibility, io, io).
:- mode do_visibility_callback(pred(in, di, uo) is det, in, di, uo) is det.

do_visibility_callback(VisibilityFunc, Visibility, !IO) :-
    VisibilityFunc(Visibility, !IO).

:- pragma foreign_proc("C",
    disable_visibility_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure, tabled_for_io],
"
    glutVisibilityFunc(NULL);
").

:- pragma foreign_enum("C", visibility/0,
[
    visible     - "GLUT_VISIBLE",
    not_visible - "GLUT_NOT_VISIBLE"
]).

%-----------------------------------------------------------------------------%
%
% Idle callback
%

:- pragma foreign_proc("C",
    idle_func(Closure::pred(di, uo) is det, _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure, tabled_for_io],
"
    mglut_idle_callback = Closure;
    glutIdleFunc(MGLUT_idle_callback);
").

:- pragma foreign_code("C", "
void MGLUT_idle_callback(void)
{
    MGLUT_do_idle_callback(mglut_idle_callback);
}").

:- pragma foreign_export("C",
    do_idle_callback(pred(di, uo) is det, di, uo),
    "MGLUT_do_idle_callback").
:- pred do_idle_callback(pred(io, io), io, io).
:- mode do_idle_callback(pred(di, uo) is det, di, uo) is det.

do_idle_callback(IdleFunc, !IO) :- IdleFunc(!IO).

:- pragma foreign_proc("C",
    disable_idle_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure, tabled_for_io],
"
    glutIdleFunc(NULL);
").

%-----------------------------------------------------------------------------%
%
% Keyboard up callbacks
%

:- pragma foreign_proc("C",
    keyboard_up_func(KeyUpFunc::pred(in, in, in, di ,uo) is det, _IO0::di,
        _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_keyboard_up_callback = KeyUpFunc;
    glutKeyboardUpFunc(MGLUT_keyboard_up_callback);
").

:- pragma foreign_code("C", "
void MGLUT_keyboard_up_callback(unsigned char scan_code, int x, int y)
{
    MGLUT_do_keyboard_up_callback(mglut_keyboard_up_callback,
        (MR_Char) scan_code, (MR_Integer) x, (MR_Integer) y);
}").

:- pragma foreign_export("C",
    do_keyboard_up_callback(pred(in, in, in, di, uo) is det,
        in, in, in, di, uo),
    "MGLUT_do_keyboard_up_callback").
:- pred do_keyboard_up_callback(pred(char, int, int, io, io), char, int, int,
    io, io).
:- mode do_keyboard_up_callback(pred(in,in,in,di,uo) is det, in, in, in, di,
    uo) is det.

do_keyboard_up_callback(KeyBoardUpFunc, ScanCode, X, Y, !IO) :-
    KeyBoardUpFunc(ScanCode, X, Y, !IO).

:- pragma foreign_proc("C",
    disable_keyboard_up_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    glutKeyboardFunc(NULL);
").

%-----------------------------------------------------------------------------%
%
% Overlay display callbacks
%

:- pragma foreign_proc("C",
    overlay_display_func(OverlayFunc::pred(di, uo) is det, _IO0::di, _IO::uo),
    [may_call_mercury, promise_pure, terminates],
"
    mglut_overlay_display_callback = OverlayFunc;
    glutOverlayDisplayFunc(MGLUT_overlay_display_callback);
").

:- pragma foreign_code("C", "
void MGLUT_overlay_display_callback(void)
{
    MGLUT_do_overlay_display_callback(mglut_overlay_display_callback);
}").

:- pragma foreign_export("C",
    do_overlay_display_callback(pred(di, uo) is det, di, uo),
    "MGLUT_do_overlay_display_callback").
:- pred do_overlay_display_callback(pred(io, io), io, io).
:- mode do_overlay_display_callback(pred(di, uo) is det, di, uo) is det.

do_overlay_display_callback(OverlayDisplayFunc, !IO) :-
    OverlayDisplayFunc(!IO).

:- pragma foreign_proc("C",
    overlay_display_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    glutOverlayDisplayFunc(NULL);
").

%-----------------------------------------------------------------------------%
%
% Special keyboard callbacks
%

:- pragma foreign_proc("C",
    callback.special_func(SpecialFunc::pred(in, in, in, di, uo) is det,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_special_callback = SpecialFunc;
    glutSpecialFunc(MGLUT_special_callback);
").

:- pragma foreign_code("C", "
void MGLUT_special_callback(int key, int x, int y)
{
    MGLUT_do_special_callback(mglut_special_callback, (MR_Integer) key,
        (MR_Integer) x, (MR_Integer) y);
}
").

:- pragma foreign_export("C",
    do_special_callback(pred(in, in, in ,di, uo) is det,
        in, in, in, di, uo),
    "MGLUT_do_special_callback").
:- pred do_special_callback(pred(special_key, int, int, io, io),
    special_key, int, int, io, io).
:- mode do_special_callback(pred(in, in, in, di, uo) is det,
    in, in, in, di, uo) is det.

do_special_callback(Special, Key, X, Y, !IO) :-
    Special(Key, X, Y, !IO).

:- pragma foreign_proc("C",
    callback.disable_special_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    glutSpecialFunc(NULL);
").

%-----------------------------------------------------------------------------%
%
% Special keyboard up callbacks
%

:- pragma foreign_proc("C",
    callback.special_up_func(SpecialFunc::pred(in, in, in, di, uo) is det,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, tabled_for_io, promise_pure],
"
    mglut_special_up_callback = SpecialFunc;
    glutSpecialUpFunc(MGLUT_special_up_callback);
").

:- pragma foreign_code("C", "
void MGLUT_special_up_callback(int key, int x, int y)
{
    MGLUT_do_special_up_callback(mglut_special_up_callback,
        (MR_Integer) key, (MR_Integer) x, (MR_Integer) y);
}").

:- pragma foreign_export("C",
    do_special_up_callback(pred(in, in, in ,di, uo) is det,
        in, in, in, di, uo),
    "MGLUT_do_special_up_callback").
:- pred do_special_up_callback(pred(special_key, int, int, io, io),
        special_key, int, int, io, io).
:- mode do_special_up_callback(pred(in,in,in,di,uo) is det, in, in, in, di, uo)
        is det.

do_special_up_callback(SpecialUpFunc, Key, X, Y, !IO) :-
    SpecialUpFunc(Key, X, Y, !IO).

:- pragma foreign_proc("C",
    callback.disable_special_up_func(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure],
"
    glutSpecialUpFunc(NULL);
").

%-----------------------------------------------------------------------------%

:- pragma foreign_enum("C", special_key/0, [
    f1          - "GLUT_KEY_F1",
    f2          - "GLUT_KEY_F2",
    f3          - "GLUT_KEY_F3",
    f4          - "GLUT_KEY_F4",
    f5          - "GLUT_KEY_F5",
    f6          - "GLUT_KEY_F6",
    f7          - "GLUT_KEY_F7",
    f8          - "GLUT_KEY_F8",
    f9          - "GLUT_KEY_F9",
    f10         - "GLUT_KEY_F10",
    f11         - "GLUT_KEY_F11",
    f12         - "GLUT_KEY_F12",
    left        - "GLUT_KEY_LEFT",
    up          - "GLUT_KEY_UP",
    right       - "GLUT_KEY_RIGHT",
    down        - "GLUT_KEY_DOWN",
    page_up     - "GLUT_KEY_PAGE_UP",
    page_down   - "GLUT_KEY_PAGE_DOWN",
    home        - "GLUT_KEY_HOME",
    end         - "GLUT_KEY_END",
    insert      - "GLUT_KEY_INSERT"
]).

%-----------------------------------------------------------------------------%
:- end_module glut.callback.
%-----------------------------------------------------------------------------%
