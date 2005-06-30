%-----------------------------------------------------------------------------%
% Copyright (C) 2004-2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: glut.callback.m
% author: juliensf
%
% This module contains predicates for (un)registering glut callbacks.
%
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

	% Registers the rehsape callback for the current window.
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
	% This is called whenver a key is pressed.
	% The keyboard callback is Keyboard(Key, X, Y, !IO).  `Key'
	% is the ASCII value of the key pressed.  `X' and `Y' are the
	% mouse coordinates at the time the key is pressed.
	%
:- pred callback.keyboard_func(pred(char, int, int, io, io), io, io).
:- mode callback.keyboard_func(pred(in, in, in, di, uo) is det, di, uo) is det.

	% Unregisters the keyboard callback for the current window.
	%
:- pred callback.disable_keyboard_func(io::di, io::uo) is det.

:- type button ---> left ; middle ; right.

:- type button_state ---> up ; down.

	% Registers the mouse callback for the current window.
	% This is called whenever the state of one of the mouse buttons
	% changes (ie. a button is pressed or released).  The mouse
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

:- type entry_state ---> left ;	entered.

	% Registers the entry callback for the current window.
	% This is called whenever the mouse pointer enters/leaves the
	% current window.
	%
:- pred callback.entry_func(pred(entry_state, io, io), io, io).
:- mode callback.entry_func(pred(in, di, uo) is det, di, uo) is det.

	% Unregisters the entry callback for the current window.
	%
:- pred callback.disable_entry_func(io::di, io::uo) is det.

:- type visibility --->	visible ; not_visible.

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
	--->	f1
	;	f2
	; 	f3
	;	f4
	;	f5
	;	f6
	;	f7
	;	f8
	;	f9
	;	f10
	;	f11
	;	f12
	;	left
	;	up
	;	right
	;	down
	;	page_up
	;	page_down
	;	home
	;	end
	;	insert.

	% Register the special keyboard callback for the current window.
	% This is called when one of the function or arrow keys is pressed.
	%
:- pred callback.special_func(pred(special_key, int, int, io, io), io, io).
:- mode callback.special_func(pred(in, in, in, di, uo) is det, di, uo) is det.

	% Unregister the special keyboard callback for the current window.
	%
:- pred callback.disable_special_func(io::di, io::uo) is det.

	% Register the special keyboard up callbcak for the current window.
	% This is called when one fo the function or arrow keys is released.
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

	% Unregister the keyuboard_up callback for the current window.
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
	
	/* XXX If we ever support multiple windows remember that the idle
	 * callback is global.
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
% Display callbacks.
%

:- pragma foreign_proc("C",
	callback.display_func(DisplayFunc::pred(di, uo) is det, IO0::di,
		IO::uo),
	[may_call_mercury, promise_pure, terminates],
"
	mglut_display_callback = DisplayFunc;
	glutDisplayFunc(MGLUT_display_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_display_callback(void)
{
	MGLUT_do_display_callback(mglut_display_callback);	
}
").

:- pragma export(do_display_callback(pred(di, uo) is det, di, uo),
	"MGLUT_do_display_callback").
:- pred do_display_callback(pred(io, io), io, io).
:- mode do_display_callback(pred(di, uo) is det, di, uo) is det.

do_display_callback(DisplayFunc, !IO) :- DisplayFunc(!IO).

:- pragma foreign_proc("C",
	disable_display_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutDisplayFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Reshape callbacks.
%

:- pragma foreign_proc("C",
	callback.reshape_func(Reshape::pred(in, in, di, uo) is det, IO0::di, 
		IO::uo),
	[may_call_mercury, promise_pure, terminates],
"
	mglut_reshape_callback = Reshape;
	glutReshapeFunc(MGLUT_reshape_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_reshape_callback(int width, int height)
{
	MGLUT_do_reshape_callback(mglut_reshape_callback, width, height);	
}
").

:- pragma export(do_reshape_callback(pred(in, in, di, uo) is det, in, in, di, 
	uo), "MGLUT_do_reshape_callback").
:- pred do_reshape_callback(pred(int, int, io, io), int, int, io, io).
:- mode do_reshape_callback(pred(in, in, di, uo) is det, in, in, di, uo) is det.

do_reshape_callback(ReshapeFunc, Width, Height, !IO) :-
	ReshapeFunc(Width, Height, !IO).

:- pragma foreign_proc("C",
	disable_reshape_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutReshapeFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Keyboard callbacks.
%

:- pragma foreign_proc("C",
	keyboard_func(KeyboardFunc::pred(in, in, in, di, uo) is det, IO0::di, 
		IO::uo),
	[may_call_mercury, promise_pure, terminates],
"
	mglut_keyboard_callback = KeyboardFunc;
	glutKeyboardFunc(MGLUT_keyboard_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_keyboard_callback(unsigned char scan_code, int x, int y)
{
	MGLUT_do_keyboard_callback(mglut_keyboard_callback,
		(MR_Char) scan_code, (MR_Integer) x, (MR_Integer) y);	
}
").

:- pragma export(do_keyboard_callback(pred(in, in, in, di, uo) is det, 
	in, in, in, di, uo), "MGLUT_do_keyboard_callback").
:- pred do_keyboard_callback(pred(char, int, int, io, io), char, int, int, 
	io, io).
:- mode do_keyboard_callback(pred(in, in, in, di, uo) is det, in, in, in,
	di, uo) is det.

do_keyboard_callback(KeyBoardFunc, ScanCode, X, Y, !IO) :-
	KeyBoardFunc(ScanCode, X, Y, !IO).

:- pragma foreign_proc("C",
	disable_keyboard_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutKeyboardFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Mouse callbacks.
%

:- pragma foreign_proc("C",
	mouse_func(MouseFunc::pred(in, in, in, in, di, uo) is det, IO0::di, 
		IO::uo),
	[may_call_mercury, promise_pure, terminates],
"
	mglut_mouse_callback = MouseFunc;
	glutMouseFunc(MGLUT_mouse_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_mouse_callback(int button, int state, int x, int y)
{
	MGLUT_do_mouse_callback(mglut_mouse_callback, (MR_Integer) button, 
		(MR_Integer) state, (MR_Integer) x, (MR_Integer) y);	
}
").

:- pragma export(do_mouse_callback(pred(in, in, in, in, di, uo) is det, 
		in, in, in, in, di, uo), "MGLUT_do_mouse_callback").
:- pred do_mouse_callback(pred(button, button_state, int, int, io, io), 
		int, int, int, int, io, io).
:- mode do_mouse_callback(pred(in, in, in, in, di, uo) is det, in, in, in, 
		in, di, uo) is det.

do_mouse_callback(MouseFunc, Button0, State0, X, Y, !IO) :-
	(      if Button0 = glut_left_button 	then Button = left
	  else if Button0 = glut_middle_button 	then Button = middle
	  else if Button0 = glut_right_button 	then Button = right
	  else error("Unknown mouse button.")
	),
	(      if State0 = glut_up   then State = up
	  else if State0 = glut_down then State = down
	  else error("Unknown mouse button state.")
	),
	MouseFunc(Button, State, X, Y, !IO).

:- func glut_left_button = int.
:- pragma foreign_proc("C", glut_left_button = (V::out),
	[will_not_call_mercury, promise_pure, thread_safe], "
	V = (MR_Integer) GLUT_LEFT_BUTTON;
").

:- func glut_middle_button = int.
:- pragma foreign_proc("C", glut_middle_button = (V::out),
	[will_not_call_mercury, promise_pure, thread_safe], "
	V = (MR_Integer) GLUT_MIDDLE_BUTTON;
").

:- func glut_right_button = int.
:- pragma foreign_proc("C", glut_right_button = (V::out),
	[will_not_call_mercury, promise_pure, thread_safe], "
	V = (MR_Integer) GLUT_RIGHT_BUTTON;
").

:- func glut_up = int.
:- pragma foreign_proc("C", glut_up = (V::out),
	[will_not_call_mercury, promise_pure, thread_safe], "
	V = (MR_Integer) GLUT_UP;
").

:- func glut_down = int.
:- pragma foreign_proc("C", glut_down = (V::out),
	[will_not_call_mercury, promise_pure, thread_safe], "
	V = (MR_Integer) GLUT_DOWN;
").

:- pragma foreign_proc("C", disable_mouse_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure], "
	glutMouseFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Motion callback.
%

:- pragma foreign_proc("C",
	motion_func(MotionFunc::pred(in, in, di, uo) is det, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, terminates],
"
	mglut_motion_callback = MotionFunc;
	glutMotionFunc(MGLUT_motion_callback);	
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_motion_callback(int x, int y)
{
	MGLUT_do_motion_callback(mglut_motion_callback, (MR_Integer) x, 
		(MR_Integer) y);	
}
").

:- pragma export(do_motion_callback(pred(in, in, di, uo) is det, in, in, 
	di, uo), "MGLUT_do_motion_callback").
:- pred do_motion_callback(pred(int, int, io, io), int, int, io, io).
:- mode do_motion_callback(pred(in, in, di, uo) is det, in, in, di, uo) is det. 

do_motion_callback(MotionFunc, X, Y, !IO) :- MotionFunc(X, Y, !IO).

:- pragma foreign_proc("C",
	disable_motion_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutMotionFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Passive motion callbacks.
%

:- pragma foreign_proc("C",
	passive_motion_func(PassiveMotionFunc::pred(in, in, di, uo) is det, 
		IO0::di, IO::uo),
	[may_call_mercury, promise_pure, terminates],
" 
	mglut_passive_motion_callback = PassiveMotionFunc;
	glutPassiveMotionFunc(MGLUT_passive_motion_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_passive_motion_callback(int x, int y)
{
	MGLUT_do_passive_motion_callback(mglut_passive_motion_callback,
		(MR_Integer) x, (MR_Integer) y);	
}
").

:- pragma export(do_passive_motion_callback(pred(in, in, di, uo) is det, 
	in, in, di, uo), "MGLUT_do_passive_motion_callback").
:- pred do_passive_motion_callback(pred(int, int, io, io), int, int, io, io).
:- mode do_passive_motion_callback(pred(in, in, di, uo) is det, in, in, 
	di, uo) is det.

do_passive_motion_callback(PassiveMotionFunc, X, Y, !IO) :-
	PassiveMotionFunc(X, Y, !IO).

:- pragma foreign_proc("C",
	disable_passive_motion_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutPassiveMotionFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
	entry_func(EntryFunc::pred(in, di, uo) is det, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	mglut_entry_callback = EntryFunc;
	glutEntryFunc(MGLUT_entry_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_entry_callback(int state) 
{
	MGLUT_do_entry_callback(mglut_entry_callback, state);
}").

:- pragma export(do_entry_callback(pred(in, di, uo) is det, in, di, uo), 
	"MGLUT_do_entry_callback").
:- pred do_entry_callback(pred(entry_state, io, io), int, io, io).
:- mode do_entry_callback(pred(in, di, uo) is det, in, di, uo) is det.

do_entry_callback(EntryFunc, State0, !IO) :-
	(      if State0 = glut_left 	then State = left
	  else if State0 = glut_entered then State = entered
	  else error("Unable to determine entry state.")
	),
	EntryFunc(State, !IO).

:- pragma foreign_proc("C", 
	disable_entry_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutEntryFunc(NULL);
	IO = IO0;
").

:- func glut_left = int.
:- pragma foreign_proc("C", glut_left = (Value::out), 
	[will_not_call_mercury, promise_pure],
"
	Value = (MR_Integer) GLUT_LEFT;
").

:- func glut_entered = int.
:- pragma foreign_proc("C", glut_entered = (Value::out),
	[will_not_call_mercury, promise_pure],
"
	Value = (MR_Integer) GLUT_ENTERED;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
	visibility_func(VisibilityFunc::pred(in, di, uo) is det, IO0::di, 
		IO::uo),
	[will_not_call_mercury, promise_pure],
"
	mglut_visibility_callback = VisibilityFunc;
	glutVisibilityFunc(MGLUT_visibility_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_visibility_callback(int state) 
{
	MGLUT_do_visibility_callback(mglut_visibility_callback, state);
}").

:- pragma export(do_visibility_callback(pred(in, di, uo) is det, in, di, uo),
	"MGLUT_do_visibility_callback").
:- pred do_visibility_callback(pred(visibility, io, io), int, io, io).
:- mode do_visibility_callback(pred(in, di, uo) is det, in, di, uo) is det.

do_visibility_callback(VisibilityFunc, State0, !IO) :-
	(      if State0 = glut_visible	    then State = visible
	  else if State0 = glut_not_visible then State = not_visible
	  else	  error("Unable to determine visibility.")
	),
	VisibilityFunc(State, !IO).

:- pragma foreign_proc("C",
	disable_visibility_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutVisibilityFunc(NULL);
	IO = IO0;
").

:- func glut_visible = int.
:- pragma foreign_proc("C", glut_visible = (Value::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_VISIBLE;
").

:- func glut_not_visible = int.
:- pragma foreign_proc("C", glut_not_visible = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_NOT_VISIBLE;
").

%-----------------------------------------------------------------------------%
%
% Idle callback.
%

:- pragma foreign_proc("C",
	idle_func(Closure::pred(di, uo) is det, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, terminates],
"
	mglut_idle_callback = Closure;
	glutIdleFunc(MGLUT_idle_callback);	
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_idle_callback(void)
{
	MGLUT_do_idle_callback(mglut_idle_callback);	
}").

:- pragma export(do_idle_callback(pred(di, uo) is det, di, uo), 
		"MGLUT_do_idle_callback").
:- pred do_idle_callback(pred(io, io), io, io).
:- mode do_idle_callback(pred(di, uo) is det, di, uo) is det.

do_idle_callback(IdleFunc, !IO) :- IdleFunc(!IO).

:- pragma foreign_proc("C",
	disable_idle_func(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutIdleFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Keyboard up callbacks.
%

:- pragma foreign_proc("C",
	keyboard_up_func(KeyUpFunc::pred(in, in, in, di ,uo) is det, IO0::di,
		IO::uo),
	[may_call_mercury, promise_pure, terminates],
"
	mglut_keyboard_up_callback = KeyUpFunc;
	glutKeyboardUpFunc(MGLUT_keyboard_up_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_keyboard_up_callback(unsigned char scan_code, int x, int y)
{
	MGLUT_do_keyboard_up_callback(mglut_keyboard_up_callback,
		(MR_Char) scan_code, (MR_Integer) x, (MR_Integer) y);	
}").

:- pragma export(do_keyboard_up_callback(pred(in, in, in, di, uo) is det, 
	in, in, in, di, uo), "MGLUT_do_keyboard_up_callback").
:- pred do_keyboard_up_callback(pred(char, int, int, io, io), char, int, int, 
	io, io).
:- mode do_keyboard_up_callback(pred(in,in,in,di,uo) is det, in, in, in, di, 
	uo) is det.

do_keyboard_up_callback(KeyBoardUpFunc, ScanCode, X, Y, !IO) :-
	KeyBoardUpFunc(ScanCode, X, Y, !IO).

:- pragma foreign_proc("C",
	disable_keyboard_up_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutKeyboardFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Overlay display callbacks.
%

:- pragma foreign_proc("C",
	overlay_display_func(OverlayFunc::pred(di, uo) is det, IO0::di, IO::uo),
	[may_call_mercury, promise_pure, terminates],
"
	mglut_overlay_display_callback = OverlayFunc;
	glutOverlayDisplayFunc(MGLUT_overlay_display_callback);
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_overlay_display_callback(void)
{
	MGLUT_do_overlay_display_callback(mglut_overlay_display_callback);	
}").

:- pragma export(do_overlay_display_callback(pred(di, uo) is det, di, uo), 
	"MGLUT_do_overlay_display_callback").
:- pred do_overlay_display_callback(pred(io, io), io, io).
:- mode do_overlay_display_callback(pred(di, uo) is det, di, uo) is det.

do_overlay_display_callback(OverlayDisplayFunc, !IO) :-
	OverlayDisplayFunc(!IO).

:- pragma foreign_proc("C",
	overlay_display_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutOverlayDisplayFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Special keyboard callbacks.
%

:- pragma foreign_proc("C",
	callback.special_func(SpecialFunc::pred(in, in, in, di, uo) is det, 
		IO0::di, IO::uo),
	[may_call_mercury, promise_pure, terminates],
"
	mglut_special_callback = SpecialFunc;
	glutSpecialFunc(MGLUT_special_callback);	
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_special_callback(int key, int x, int y)
{
	MGLUT_do_special_callback(mglut_special_callback, (MR_Integer) key, 
		(MR_Integer) x, (MR_Integer) y);	
}
").

:- pragma export(do_special_callback(pred(in, in, in ,di, uo) is det, 
	in, in, in, di, uo), "MGLUT_do_special_callback").
:- pred do_special_callback(pred(special_key, int, int, io, io), 
		int, int, int, io, io).
:- mode do_special_callback(pred(in,in,in,di,uo) is det, in, in, in, di, uo) 
		is det.

do_special_callback(Special, Key, X, Y, !IO) :-
	Special(int_to_special_key(Key), X, Y, !IO).

:- pragma foreign_proc("C",
	callback.disable_special_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSpecialFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Special keyboard up callbacks.
%

:- pragma foreign_proc("C",
	callback.special_up_func(SpecialFunc::pred(in, in, in, di, uo) is det, 
		IO0::di, IO::uo),
	[may_call_mercury, promise_pure, terminates],
"
	mglut_special_up_callback = SpecialFunc;
	glutSpecialUpFunc(MGLUT_special_up_callback);	
	IO = IO0;
").

:- pragma foreign_code("C", "
void MGLUT_special_up_callback(int key, int x, int y)
{
	MGLUT_do_special_up_callback(mglut_special_up_callback,
		(MR_Integer) key, (MR_Integer) x, (MR_Integer) y);	
}").

:- pragma export(do_special_up_callback(pred(in, in, in ,di, uo) is det, 
	in, in, in, di, uo), "MGLUT_do_special_up_callback").
:- pred do_special_up_callback(pred(special_key, int, int, io, io), 
		int, int, int, io, io).
:- mode do_special_up_callback(pred(in,in,in,di,uo) is det, in, in, in, di, uo) 
		is det.

do_special_up_callback(SpecialUpFunc, Key, X, Y, !IO) :-
	SpecialUpFunc(int_to_special_key(Key), X, Y, !IO).

:- pragma foreign_proc("C",
	callback.disable_special_up_func(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSpecialUpFunc(NULL);
	IO = IO0;
").

%-----------------------------------------------------------------------------%
%
% Constants for special keyboard callbacks.
%

:- func int_to_special_key(int) = special_key.

int_to_special_key(KeyCode) = Key :-
	(      if KeyCode = glut_key_f1        then Key = f1
	  else if KeyCode = glut_key_f2        then Key = f2
	  else if KeyCode = glut_key_f3        then Key = f3
	  else if KeyCode = glut_key_f4        then Key = f4
	  else if KeyCode = glut_key_f5        then Key = f5
	  else if KeyCode = glut_key_f6        then Key = f6
	  else if KeyCode = glut_key_f7        then Key = f7
	  else if KeyCode = glut_key_f8        then Key = f8
	  else if KeyCode = glut_key_f9        then Key = f9
	  else if KeyCode = glut_key_f10       then Key = f10
	  else if KeyCode = glut_key_f11       then Key = f11 
	  else if KeyCode = glut_key_f12       then Key = f12 
	  else if KeyCode = glut_key_left      then Key = left 
	  else if KeyCode = glut_key_up        then Key = up 
	  else if KeyCode = glut_key_right     then Key = right 
	  else if KeyCode = glut_key_down      then Key = down 
	  else if KeyCode = glut_key_page_up   then Key = page_up 
	  else if KeyCode = glut_key_page_down then Key = page_down 
	  else if KeyCode = glut_key_home      then Key = home 
	  else if KeyCode = glut_key_end       then Key = end 
	  else if KeyCode = glut_key_insert    then Key = insert 
	  
	  else error("Unknown special key encountered.")
	).

:- func glut_key_f1 = int.
:- pragma foreign_proc("C", glut_key_f1 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F1;
").

:- func glut_key_f2 = int.
:- pragma foreign_proc("C", glut_key_f2 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F2;
").

:- func glut_key_f3 = int.
:- pragma foreign_proc("C", glut_key_f3 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F3;
").

:- func glut_key_f4 = int.
:- pragma foreign_proc("C", glut_key_f4 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F4;
").

:- func glut_key_f5 = int.
:- pragma foreign_proc("C", glut_key_f5 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F5;
").

:- func glut_key_f6 = int.
:- pragma foreign_proc("C", glut_key_f6 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F6;
").

:- func glut_key_f7 = int.
:- pragma foreign_proc("C", glut_key_f7 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F7;
").

:- func glut_key_f8 = int.
:- pragma foreign_proc("C", glut_key_f8 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
" 
	V = (MR_Integer) GLUT_KEY_F8;
").

:- func glut_key_f9 = int.
:- pragma foreign_proc("C", glut_key_f9 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F9;
").

:- func glut_key_f10 = int.
:- pragma foreign_proc("C", glut_key_f10 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F10;
").

:- func glut_key_f11 = int.
:- pragma foreign_proc("C", glut_key_f11 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F11;
").

:- func glut_key_f12 = int.
:- pragma foreign_proc("C", glut_key_f12 = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_F12;
").

:- func glut_key_left = int.
:- pragma foreign_proc("C", glut_key_left = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_LEFT;
").

:- func glut_key_up = int.
:- pragma foreign_proc("C", glut_key_up = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_UP;
").

:- func glut_key_right = int.
:- pragma foreign_proc("C", glut_key_right = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_RIGHT;
").

:- func glut_key_down = int.
:- pragma foreign_proc("C", glut_key_down = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_DOWN;
").

:- func glut_key_page_up = int.
:- pragma foreign_proc("C", glut_key_page_up = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_PAGE_UP;
").

:- func glut_key_page_down = int.
:- pragma foreign_proc("C", glut_key_page_down = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_PAGE_DOWN;
").

:- func glut_key_home = int.
:- pragma foreign_proc("C", glut_key_home = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_HOME;
").

:- func glut_key_end = int.
:- pragma foreign_proc("C", glut_key_end = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_END;
").

:- func glut_key_insert = int.
:- pragma foreign_proc("C", glut_key_insert = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_KEY_INSERT;
").

%-----------------------------------------------------------------------------%
:- end_module glut.callback.
%-----------------------------------------------------------------------------%
