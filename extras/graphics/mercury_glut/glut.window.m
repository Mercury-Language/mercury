%-----------------------------------------------------------------------------%
% Copyright (C) 2004 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% file: glut.window.m
% author: juliensf
%
% This module provides an interface to the GLUT window managment API.
% GLUT supports two types of windows: top-level windows and subwindows.
% Both sorts support OpenGL rendering and GLUT callbacks.
%
% XXX We do not currently support multiple windows.
%-----------------------------------------------------------------------------%

:- module glut.window.

:- interface.

%-----------------------------------------------------------------------------%

:- type window.window.

	% window.create(Name, !IO).
	% Create a new top-level window.  
	% Sets the current window to this newly created window.
	% 
:- pred window.create(string::in, io::di, io::uo) is det.

	% window.create(Name, WindowId, !IO).
	% As for window.create/3 but return the id for the newly created
	% window.
:- pred window.create(string::in, window::out, io::di, io::uo) is det.

	% Create a subwindow.  Implicitly the the current window is set
	% to the newly created subwindow.
	% XXX We need a way to handle multiple windows for this to be useful.
	%
%:- pred window.create_subwindow(window::in, int::in, int::in, int::in,
%	int::in, window::out, io::di, io::uo) is det.
	
	% Destroy the specified window.  Does nothing if the 
	% specified window does not exist.
	% 
:- pred window.destroy(window::in, io::di, io::uo) is det.

	% Mark the current window as needing to be redisplayed.
	%
:- pred window.post_redisplay(io::di, io::uo) is det.

	% Mark the specified window as needing to be redisplayed.
	%
:- pred window.post_redisplay(window::in, io::di, io::uo) is det.

	% Perform a buffer swap on the layer in use in the current window.
	% If the window is not double-buffered this instruction is ignored.
	%
:- pred window.swap_buffers(io::di, io::uo) is det.

	% Get the identity of the current window.  Returns
	% `no' if no window exists or the current window has been destroyed.
	%
:- pred window.id(maybe(window)::out, io::di, io::uo) is det.

	% Sets the current window.
	% NOTE: There is no way of knowing if this actually worked, so
	% you need to call window.id/3 to make sure the current window
	% is now the one you expect.
	%
:- pred window.set(window::in, io::di, io::uo) is det.

	% Sets the title for current window.
	%
:- pred window.title(string::in, io::di, io::uo) is det.

	% Set the icon title for the current window.
	%
:- pred window.icon_title(string::in, io::di, io::uo) is det.

	% Request a change to the position of the current window.
	% NOTE: The window manager may choose to ignore this.
	%
:- pred window.position(int::in, int::in, io::di, io::uo) is det.

	% Request a change in the size of the current window.
	% NOTE: For top-level windows the window system is free to apply
	% its own policies to window sizing.
	%
:- pred window.reshape(int::in, int::in, io::di, io::uo) is det.

	% XXX Need support for multiple windows for this to be useful.
%:- pred window.pop(io::di, io::uo) is det.

	% XXX Need support for multiple windows for this to be useful.
%:- pred window.push(io::di, io::uo) is det.

	% Iconify the current window.
	%
:- pred window.iconify(io::di, io::uo) is det.

	% Show the current window.  
	% (It may not be visible if obscured by other shown windows).
	%
:- pred window.show(io::di, io::uo) is det.

	% Hide the current window.
	%
:- pred window.hide(io::di, io::uo) is det.

	% Requests that the current window be made full screen.
	% (What "full screen" means in this context is dependant
	%  upon the windowing system).
	%
:- pred window.full_screen(io::di, io::uo) is det.

:- type cursor
	--->	right_arrow 	% Arrow pointing up and to the right.
	;	left_arrow	% Arrow pointing up and to the left.
	;	info		% Pointing hand.
	;	destroy		% Skull and cross bones.
	;	help		% Question mark.
	;	cycle		% Arrows rotating in a circle.
	;	wait		% Wrist watch.
	;	text		% Insertion point for text.
	;	crosshair	% Simple cross-hair.
	;	up_down		% Bi-directional pointing up and down.
	;	left_right	% Bi-directional point left and right.
	;	top_side	% Arrow pointing to top side.
	;	bottom_side	% Arrow pointing to bottom side.
	;	left_side	% Arrow pointing to left side.
	;	right_side	% Arrow pointing to right side.
	;	top_left_corner		% Arrow pointing to top left corner.
	;	top_right_corner	% Arrow pointing to top right corner.
	;	bottom_right_corner	% Arrow pointing to bottom right corner.
	;	bottom_left_corner	% Arrow pointing to bottom left corner.
	;	full_crosshair		
				% Full screen cross hair cursor (if possible)
				% Otherwise the same as `crosshair'.
	;	none		% Invisible cursor.
	;	inherit.	% Use parent window's cursor.

	% Changes the cursor image for the current window.
	%
:- pred window.set_cursor(cursor::in, io::di, io::uo) is det.

	% Warps the pointer's location.
	%
:- pred window.warp_pointer(int::in, int::in, io::di, io::uo) is det.

	% Returns `yes(Id)' if `Id' is the parent window of the current
	% window; `no' if the current window is a top-level window.
	%
%:- pred window.get_parent(maybe(window)::out, io::di, io::uo) is det.

	% Returns the number of subwindows of the current window.
	%
%:- pred window.num_children(int::out, io::di, io::uo) is det.

	% Returns `yes' if the current window is double buffered and `no'
	% otherwise. 
	%
:- pred window.is_double_buffered(bool::out, io::di, io::uo) is det.

	% Returns `yes' if the current layer of the current window is stereo; 
	% `no' otherwise.
	%
:- pred window.is_stereo(bool::out, io::di, io::uo) is det.

	% Returns `yes' if the current layer of the current window is RGBA mode.
	%
:- pred window.is_rgba(bool::out, io::di, io::uo) is det.

	% Returns `yes' if the current window has an overlay established;
	% `no' otherwise.
	%
:- pred window.has_overlay(bool::out, io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%
% Window state.
%

:- type window.state
	--->	x               
				% Current X location in pixels. 
	;	y                  
				% Current Y location in pixels.
	;	window_width       
				% Width of the current window in pixels.
	;	window_height
				% Height of the current window in pixels.
	;	buffer_size        
				% Number of bits in the current layer of
				% the current window's color buffer.  
	;	stencil_size        
				% Number of bits in the current layer of
				% the current window's stencil buffer.
	;	depth_size
				% Number of bits in the current layer of
				% the current window's depth buffer.
	;	red_size
				% Number of bits of red stored in the current
				% layer of the current window's color buffer.
				% Zero if in color index mode.
	;	green_size
				% As above but the number of green bits.
	;	blue_size
				% As above but the number of blue bits.
	;	alpha_size
				% As above but the number of alpha bits.
	;	accum_red_size
				% Number of bits of red in the accumulation
				% buffer of the current layer of the current
				% window.  Zero if in color index mode.
	;	accum_green_size
				% As above but the number of green bits.	
	;	accum_blue_size
				% As above but the number of blue bits.
	;	accum_alpha_size
				% As above but the number of alpha bits.
	;	colormap_size
				% Size of the color index colormap of the 
				% current layer of the current window.
				% Zero if in RGBA mode.
	;	number_samples
				% Number of samples for multisampling for the
				% current layer of the current window.
	
	;	format_id.
				% Window system dependent format Id for the
				% current layer of the current window.

	% Return the current setting of the specified parameter for the
	% current window.
	%
:- pred window.get(window.state::in, int::out, io::di, io::uo) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_decl("C", "#include <GL/glut.h>").

:- type window == int.

%-----------------------------------------------------------------------------%

window.create(Name, !IO) :-
	window.create(Name, _, !IO).

:- pragma foreign_proc("C",
	window.create(Name::in, Win::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
" 
	Win = (MR_Integer) glutCreateWindow((char *) Name);
	IO = IO0;
").

	% XXX This will not work properly until we can handle callbacks
	% for multiple windows.  
%:- pragma foreign_proc("C",
%	create_subwindow(Parent::in, X::in, Y::in, W::in, H::in, Child::out,
%		IO0::di, IO::uo),
%	[will_not_call_mercury, promise_pure],
%"
%	Child = (MR_Integer) glutCreateSubWindow((int)Parent, (int)X, (int)Y,
%		(int)W, (int)H);
%	IO = IO0;
%").

:- pragma foreign_proc("C",
	window.destroy(Window::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutDestroyWindow(Window);
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.post_redisplay(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutPostRedisplay();
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.post_redisplay(Id::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutPostWindowRedisplay((int) Id);
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.swap_buffers(IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutSwapBuffers();
	IO = IO0;
").

window.id(MaybeWindow, !IO) :-
	window.id_2(Window, !IO), 
	MaybeWindow = ( if Window = 0 then no else yes(Window) ).

:- pred window.id_2(int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
	window.id_2(Win::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	Win = (MR_Integer) glutGetWindow();
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.set(Window::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSetWindow((int) Window);
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.title(Title::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSetWindowTitle(Title);
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.icon_title(Title::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSetIconTitle((char *) Title);
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.position(X::in, Y::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutPositionWindow((int) X, (int) Y);
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.reshape(W::in, H::in, IO0::di, IO::uo), 
	[will_not_call_mercury, promise_pure],
"
	glutReshapeWindow(W, H);
	IO = IO0;
").

%:- pragma foreign_proc("C",
%	window.pop(IO0::di, IO::uo),
%	[will_not_call_mercury, promise_pure],
%"
%	glutPopWindow();
%	IO = IO0;
%").

%:- pragma foreign_proc("C",
%	window.push(IO0::di, IO::uo),
%	[will_not_call_mercury, promise_pure],
%"
%	glutPushWindow();
%	IO = IO0;
%").

:- pragma foreign_proc("C",
	window.iconify(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutIconifyWindow();
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.show(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutShowWindow();
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.hide(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutHideWindow();
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.full_screen(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutFullScreen();
	IO = IO0;
").

:- func cursor_to_int(cursor) = int.

cursor_to_int(right_arrow) = glut_cursor_right_arrow.
cursor_to_int(left_arrow)  = glut_cursor_left_arrow.
cursor_to_int(info)        = glut_cursor_info.
cursor_to_int(destroy)     = glut_cursor_destroy.
cursor_to_int(help)        = glut_cursor_help.
cursor_to_int(cycle)       = glut_cursor_cycle.
cursor_to_int(wait)        = glut_cursor_wait.
cursor_to_int(text)        = glut_cursor_text.
cursor_to_int(crosshair)   = glut_cursor_crosshair.
cursor_to_int(up_down)     = glut_cursor_up_down.
cursor_to_int(left_right)  = glut_cursor_left_right.
cursor_to_int(top_side)    = glut_cursor_top_side.
cursor_to_int(bottom_side) = glut_cursor_bottom_side.
cursor_to_int(left_side)   = glut_cursor_left_side.
cursor_to_int(right_side)  = glut_cursor_right_side.
cursor_to_int(top_left_corner)   = glut_cursor_top_left_corner.
cursor_to_int(top_right_corner)  = glut_cursor_top_right_corner.
cursor_to_int(bottom_right_corner) = glut_cursor_bottom_right_corner.
cursor_to_int(bottom_left_corner)  = glut_cursor_bottom_left_corner.
cursor_to_int(full_crosshair)      = glut_cursor_full_crosshair.
cursor_to_int(none)    = glut_cursor_none.
cursor_to_int(inherit) = glut_cursor_inherit.

:- func glut_cursor_right_arrow = int.
:- pragma foreign_proc("C", glut_cursor_right_arrow = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_RIGHT_ARROW;
").

:- func glut_cursor_left_arrow = int.
:- pragma foreign_proc("C", glut_cursor_left_arrow = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_LEFT_ARROW;
").

:- func glut_cursor_info = int.
:- pragma foreign_proc("C", glut_cursor_info = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_INFO;
").

:- func glut_cursor_destroy = int.
:- pragma foreign_proc("C", glut_cursor_destroy = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_DESTROY;
").

:- func glut_cursor_help = int.
:- pragma foreign_proc("C", glut_cursor_help = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_HELP;
").

:- func glut_cursor_cycle = int.
:- pragma foreign_proc("C", glut_cursor_cycle = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_CYCLE;
").
:- func glut_cursor_wait = int.
:- pragma foreign_proc("C", glut_cursor_wait = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_WAIT;
").

:- func glut_cursor_text = int.
:- pragma foreign_proc("C", glut_cursor_text = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_TEXT;
").

:- func glut_cursor_crosshair = int.
:- pragma foreign_proc("C", glut_cursor_crosshair = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_CROSSHAIR;
").

:- func glut_cursor_up_down = int.
:- pragma foreign_proc("C", glut_cursor_up_down = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_UP_DOWN;
").

:- func glut_cursor_left_right = int.
:- pragma foreign_proc("C", glut_cursor_left_right = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_LEFT_RIGHT;
").

:- func glut_cursor_top_side = int.
:- pragma foreign_proc("C", glut_cursor_top_side = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_TOP_SIDE;
").

:- func glut_cursor_bottom_side = int.
:- pragma foreign_proc("C", glut_cursor_bottom_side = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_BOTTOM_SIDE;
").

:- func glut_cursor_left_side = int.
:- pragma foreign_proc("C", glut_cursor_left_side = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_LEFT_SIDE;
").

:- func glut_cursor_right_side = int.
:- pragma foreign_proc("C", glut_cursor_right_side = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_RIGHT_SIDE;
").

:- func glut_cursor_top_left_corner = int.
:- pragma foreign_proc("C", glut_cursor_top_left_corner = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_TOP_LEFT_CORNER;
").

:- func glut_cursor_top_right_corner = int.
:- pragma foreign_proc("C", glut_cursor_top_right_corner = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_TOP_RIGHT_CORNER;
").

:- func glut_cursor_bottom_right_corner = int.
:- pragma foreign_proc("C", glut_cursor_bottom_right_corner = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_BOTTOM_RIGHT_CORNER;
").

:- func glut_cursor_bottom_left_corner = int.
:- pragma foreign_proc("C", glut_cursor_bottom_left_corner = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_BOTTOM_LEFT_CORNER;
").

:- func glut_cursor_full_crosshair = int.
:- pragma foreign_proc("C", glut_cursor_full_crosshair = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_FULL_CROSSHAIR;
").

:- func glut_cursor_none = int.
:- pragma foreign_proc("C", glut_cursor_none = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_NONE;
").

:- func glut_cursor_inherit = int.
:- pragma foreign_proc("C", glut_cursor_inherit = (V::out), 
	[will_not_call_mercury, promise_pure, thread_safe],
"
	V = (MR_Integer) GLUT_CURSOR_INHERIT;
").

window.set_cursor(Cursor, !IO) :-
	window.set_cursor_2(cursor_to_int(Cursor), !IO).

:- pred window.set_cursor_2(int::in, io::di, io::uo) is det.
:- pragma foreign_proc("C",
	window.set_cursor_2(Cursor::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutSetCursor((int) Cursor);
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.warp_pointer(X::in, Y::in, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	glutWarpPointer(X, Y);
	IO = IO0;
").

%-----------------------------------------------------------------------------%

%window.get_parent(Result, !IO) :-
%	get_parent_2(Result0, !IO),
%	Result = ( if Result0 = 0 then no else yes(Result0) ).
%
%:- pred get_parent_2(int::out, io::di, io::uo) is det.
%:- pragma foreign_proc("C",
%	get_parent_2(Result::out, IO0::di, IO::uo),
%	[will_not_call_mercury, promise_pure],
%"
%	Result = (MR_Integer) glutGet(GLUT_WINDOW_PARENT);
%	IO = IO0;
%").

%:- pragma foreign_proc("C",
%	window.num_children(Result::out, IO0::di, IO::uo),
%	[will_not_call_mercury, promise_pure],
%"
%	Result = (MR_Integer) glutGet(GLUT_WINDOW_NUM_CHILDREN);
%	IO = IO0;
%").

:- pragma foreign_proc("C",
	window.is_double_buffered(DB::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	if (glutGet(GLUT_WINDOW_DOUBLEBUFFER)) {
		DB = MR_YES;
	} else {
		DB = MR_NO;
	}
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.is_stereo(Stereo::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	if (glutGet(GLUT_WINDOW_STEREO)) {
		Stereo = MR_YES;
	} else {
		Stereo = MR_NO;
	}
	IO = IO0;
").

:- pragma foreign_proc("C",
	window.is_rgba(RGBA::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	if (glutGet(GLUT_WINDOW_RGBA)) {
		RGBA = MR_YES;
	} else {
		RGBA = MR_NO;
	}
	IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func window_state_to_int(window.state) = int.

window_state_to_int(x)                = glut_window_x.              
window_state_to_int(y)                = glut_window_y.                 
window_state_to_int(window_width)     = glut_window_width.      
window_state_to_int(window_height)    = glut_window_height.     
window_state_to_int(buffer_size)      = glut_window_buffer_size.      
window_state_to_int(stencil_size)     = glut_window_stencil_size.      
window_state_to_int(depth_size)       = glut_window_depth_size.       
window_state_to_int(red_size)         = glut_window_red_size.
window_state_to_int(green_size)       = glut_window_green_size.
window_state_to_int(blue_size)        = glut_window_blue_size.
window_state_to_int(alpha_size)       = glut_window_alpha_size.
window_state_to_int(accum_red_size)   = glut_window_accum_red_size.
window_state_to_int(accum_green_size) = glut_window_accum_green_size.
window_state_to_int(accum_blue_size)  = glut_window_accum_blue_size.
window_state_to_int(accum_alpha_size) = glut_window_accum_alpha_size.
window_state_to_int(colormap_size)    = glut_window_colormap_size.
window_state_to_int(number_samples)   = glut_window_num_samples.
window_state_to_int(format_id)        = glut_window_format_id.

window.get(State, Value, !IO) :-
	window.get_2(window_state_to_int(State), Value, !IO).

:- pred window.get_2(int::in, int::out, io::di, io::uo) is det.
:- pragma foreign_proc("C",
	window.get_2(State::in, Value::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	Value = (MR_Integer) glutGet((GLenum) State);
	IO = IO0;
").

%-----------------------------------------------------------------------------%

:- func glut_window_x = int.
:- pragma foreign_proc("C", glut_window_x = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_X;
").

:- func glut_window_y = int.
:- pragma foreign_proc("C", glut_window_y = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_Y;
").

:- func glut_window_width = int.
:- pragma foreign_proc("C", glut_window_width = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_WIDTH;
").

:- func glut_window_height = int.
:- pragma foreign_proc("C", glut_window_height = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_HEIGHT;
").

:- func glut_window_buffer_size = int.
:- pragma foreign_proc("C", glut_window_buffer_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_BUFFER_SIZE;
").

:- func glut_window_stencil_size = int.
:- pragma foreign_proc("C", glut_window_stencil_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_STENCIL_SIZE;
").

:- func glut_window_depth_size = int.
:- pragma foreign_proc("C", glut_window_depth_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_DEPTH_SIZE;
").

:- func glut_window_red_size = int.
:- pragma foreign_proc("C", glut_window_red_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_RED_SIZE;
").

:- func glut_window_green_size = int.
:- pragma foreign_proc("C", glut_window_green_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_GREEN_SIZE;
").

:- func glut_window_blue_size = int.
:- pragma foreign_proc("C", glut_window_blue_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_BLUE_SIZE;
").

:- func glut_window_alpha_size = int.
:- pragma foreign_proc("C", glut_window_alpha_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_ALPHA_SIZE;
").

:- func glut_window_accum_red_size = int.
:- pragma foreign_proc("C", glut_window_accum_red_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_ACCUM_RED_SIZE;
").

:- func glut_window_accum_green_size = int.
:- pragma foreign_proc("C", glut_window_accum_green_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_ACCUM_GREEN_SIZE;
").

:- func glut_window_accum_blue_size = int.
:- pragma foreign_proc("C", glut_window_accum_blue_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_ACCUM_BLUE_SIZE;
").

:- func glut_window_accum_alpha_size = int.
:- pragma foreign_proc("C", glut_window_accum_alpha_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_ACCUM_ALPHA_SIZE;
").

:- func glut_window_colormap_size = int.
:- pragma foreign_proc("C", glut_window_colormap_size = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_COLORMAP_SIZE;
").

:- func glut_window_num_samples = int.
:- pragma foreign_proc("C", glut_window_num_samples = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_NUM_SAMPLES;
").

:- func glut_window_format_id = int.
:- pragma foreign_proc("C", glut_window_format_id = (Value::out),
	[will_not_call_mercury, promise_pure, thread_safe],
"
	Value = (MR_Integer) GLUT_WINDOW_FORMAT_ID;
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("C",
	window.has_overlay(Result::out, IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	if (glutLayerGet(GLUT_HAS_OVERLAY)) {
		Result = MR_YES;
	} else {
		Result = MR_NO;
	}
	IO = IO0;
").

%-----------------------------------------------------------------------------%
:- end_module glut.window.
%-----------------------------------------------------------------------------%
